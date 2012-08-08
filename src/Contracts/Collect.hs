{-

    This module collects the contracts in the source file and turns
    them into hcc's internal reperesantion of contracts,
    i.e. translating the higher order abstract syntax that is used.

    The inliner must be run before, otherwise this will throw errors

-}
module Contracts.Collect where

import CoreFVs
import CoreSyn
import CoreUtils
import Id
import Name
import SrcLoc
import Type
import UniqSet
import UniqSupply
import Var hiding (varName)

import Contracts.SrcRep
import Contracts.Types
import Contracts.Theory

import Halo.Util
import Halo.Shared
import Halo.Subtheory
import Halo.FreeTyCons
import Halo.Class

import Data.Either
import Data.List

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.State

collectContracts :: [CoreBind]
                 -> UniqSM (Either String ([TopStmt],[CoreBind],[String]))
collectContracts program = runErrorT $ do
    let program' :: [CoreBind]
        untranslated_stmts :: [(Var,CoreExpr)]
        (program',untranslated_stmts) = partitionEithers $ map pick program
          where
            pick :: CoreBind -> Either CoreBind (Var,CoreExpr)
            pick b = maybe (Left b) Right . msum . map (uncurry pick') $ vses
              where vses = flattenBinds [b]

            pick' :: Var -> CoreExpr -> Maybe (Var,CoreExpr)
            pick' v e | isStatementType v && isExportedId v = Just (v,e)
            pick' _ _                                       = Nothing

    (stmts,db_msgs) <- runCollectM $ do
        write "Collecting statements:"
        write (unlines (map (show . fst) untranslated_stmts))
        mapM (uncurry mkTopStmt) untranslated_stmts

    return (stmts,program',db_msgs)

type CollectM =
    (WriterT [String]
    -- ^ Writing debug messages
    (StateT [String]
    -- ^ State of variable names
    (ErrorT String
    -- ^ We can error by throwing a string message
    UniqSM)))
    -- ^ Making variable names

runCollectM :: CollectM a -> ErrorT String UniqSM (a,[String])
runCollectM m = runWriterT m `evalStateT` names
  where
    names = (`replicateM` ['a'..'z']) =<< [1..]

trimTyApp :: CoreExpr -> Maybe (CoreExpr,[CoreExpr])
trimTyApp e@App{} = Just (second trimTyArgs (collectArgs e))
trimTyApp e@Var{} = Just (e,[])
trimTyApp e@Lam{} = Just (e,[])
trimTyApp _       = Nothing

mkTopStmt :: Var -> CoreExpr -> CollectM TopStmt
mkTopStmt name e = do
    write $ "Making a top statement for " ++ show name
    let fun_deps :: [HCCContent]
        fun_deps = Function <$> filter (not . isTyVar) (uniqSetToList (exprFreeVars e))
    write $ "Fundeps: " ++ show fun_deps
    (ty_deps,stmt) <- mkStatement atTop e
    write $ "Tydeps: " ++ show ty_deps
    let deps     = fun_deps ++ ty_deps
    return $ TopStmt name stmt deps

inTree,atTop :: Bool
inTree = True
atTop = False

mkStatement :: Bool -> CoreExpr -> CollectM ([HCCContent],Statement)
mkStatement in_tree e = do
    let (_ty,args,e_stripped) = collectTyAndValBinders e
    write $ "Translating statement " ++ showExpr e ++ " with arguments " ++ show args
    case collectArgs e_stripped of
        (Var x,[_c_ty,f,c]) | isStatementCon x -> do
            write $ "A contract for: " ++ showExpr f ++ "."
            contr <- mkContract f c
            let ty_deps = Data <$> freeTyCons f :: [HCCContent]
            write $ "Tydeps: " ++ show ty_deps
            let dict_deps = dictDeps f :: [HCCContent]
            write $ "Dict deps: " ++ show dict_deps
            return $ (ty_deps ++ dict_deps,Statement f contr args [])
        (Var x,[s,u]) | isStatementUsing x ->
            if in_tree
                then do
                    write $ "A skipped tree using: " ++ showExpr u
                    (t,s') <- mkStatement inTree s
                    return (t,s' { statement_args = args ++ statement_args s' })
                else do
                    write $ "A contract using: " ++ showExpr u
                    (ty_deps_s,s') <- mkStatement atTop s
                    (ty_deps_u,u') <- mkStatement inTree u
                    let ty_deps = ty_deps_s `union` ty_deps_u
                    write $ "Tydeps: " ++ show ty_deps
                    return $ (ty_deps,s' { statement_using = u' : statement_using s'
                                         , statement_args  = args ++ statement_args s'
                                         })
        _ -> throw $ "Error: Invalid statement " ++ showExpr e_stripped

-- | Updates the unique in a Var: i.e. make it different from the one
--   we had, but otherwise identical.
refresh :: Var -> CollectM Var
refresh v = setVarUnique v <$> (lift $ lift $ lift $ getUniqueM)

mkContract :: CoreExpr -> CoreExpr -> CollectM Contract
mkContract f e = do
    write $ showExpr f ++ ": " ++ showExpr e
    case collectArgs e of
        (Var x,[_cf_ty])     | isContrCF x -> return CF
        (Var x,[_pred_ty,p]) | isContrPred x -> return (Pred (p @@ f))

        (Var x,[_c1_ty,_c2_ty,e1,Lam y e2]) | isContrPi x -> do
            y' <- refresh y
            Arrow y' <$> mkContract (Var y') (subst e1 y y')
                     <*> mkContract (subst f y y' @@ Var y') (subst e2 y y')

        (Var x,[Type c1_ty,_c2_ty,e1,e2])
            | isContrArr x -> do
                v <- mkFreshVar c1_ty
                Arrow v <$> mkContract (Var v) e1
                        <*> mkContract (f @@ Var v) e2

        (Var x,[_and_ty,e1,e2])
            | isContrAnd x -> And <$> mkContract f e1 <*> mkContract f e2

        (Lam x e',[]) | isTyVar x -> do
            write $ "Skipping lambda of a type variable"
            mkContract f e'

        t -> throw $ "Found weird expression " ++ showExpr e ++
                     " when making a contract for " ++ showExpr f ++
                     "\n  current trimmedTyApp was" ++ showOutputable t

-- from e :: Contract a
-- to Contract a
-- to (Contract,a)
-- to a
contractType :: CoreExpr -> Type
contractType = snd . splitAppTy . exprType

mkFreshVar :: Type -> CollectM Var
mkFreshVar ty = do
    v <- gets head
    write $ "Making a fresh variable " ++ v ++ " with type " ++ showOutputable ty
    modify tail
    u <- lift $ lift $ lift $ getUniqueM
    let name = mkInternalName u (mkOccName varName v) wiredInSrcSpan
    return (mkVanillaGlobal name ty)

write :: String -> CollectM ()
write = tell . return

throw :: String -> CollectM a
throw = throwError . strMsg

