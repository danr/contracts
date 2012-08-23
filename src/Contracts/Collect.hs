{-# LANGUAGE PatternGuards #-}
{-

    This module collects the contracts in the source file and turns
    them into hcc's internal reperesantion of contracts,
    i.e. translating the higher order abstract syntax that is used.

    The inliner must be run before, otherwise this will throw errors

-}
module Contracts.Collect where

import CoreSyn
import Id
import Name
import SrcLoc
import Type
import UniqSupply
import Unique
import Var hiding (varName)

import Contracts.SrcRep
import Contracts.Types

import Halo.Util
import Halo.Shared

import Data.Either

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
    stmt <- unTreeStmt <$> mkStatement e
    return $ TopStmt
        { top_name = name
        , top_stmt = stmt
        , top_deps = stmtDeps stmt
        }

mkStatement :: CoreExpr -> CollectM Statement
mkStatement e = do
    let (_ty,args,e_stripped) = collectTyAndValBinders e
    unless (null args) $ throw $ unlines
        ["Cannot have arguments to a Statement"
        ,showExpr e
        ,"(perhaps you want to use the All constructor explicitly?"]
    write $ "Translating statement " ++ showExpr e
    case collectArgs e_stripped of

        (Var x,as)
            | isStatementAll x , not (null as) , Lam y s <- last as -> do
                write $ "Binding " ++ show y ++ " in a statement " ++ showExpr s
                mkAll y <$> mkStatement s

        (Var x,[_c_ty,f,c])
            | isStatementCon x -> do
                write $ "A contract for: " ++ showExpr f ++ "."
                (f :::) <$> mkContract f c

        (Var x,[Type _st_ty,Type _s_ty,Type _u_ty,Coercion _co,s,u])
            | isStatementAssuming x -> do
                write $ "Assuming " ++ showExpr s ++ " in the statement " ++ showExpr u
                (:=>) <$> mkStatement s <*> mkStatement u

        (Var x,[Type _st_ty,Type _s_ty,Type _u_ty,Coercion _co,s,u])
            | isStatementUsing x -> do
                write $ "A contract using: " ++ showExpr u
                Using <$> mkStatement s <*> mkStatement u

        t -> throw $ "Cannot translate this statement: " ++ showExpr e_stripped ++
                     "\n  current collectedArgs was" ++ showOutputable t

mkContract :: CoreExpr -> CoreExpr -> CollectM Contract
mkContract f e = do
    write $ showExpr f ++ ": " ++ showExpr e
    case collectArgs e of
        (Var x,[_cf_ty])     | isContrCF x -> return CF
        (Var x,[_pred_ty,p]) | isContrPred x -> return (Pred (p @@ f))

        -- I don't know why both these appear
        (Var x,[Type c1_ty,Type _c2_ty,e1,Lam y e2])
            | isContrPi x -> do
                y' <- refresh y c1_ty
                Arrow y' <$> mkContract (Var y') (subst e1 y y')
                         <*> mkContract (subst f y y' @@ Var y') (subst e2 y y')

        (Var x,[Type _c_ty,Type c1_ty,Type _c2_ty,Coercion _co,e1,Lam y e2])
            | isContrPi x -> do
                y' <- refresh y c1_ty
                Arrow y' <$> mkContract (Var y') (subst e1 y y')
                         <*> mkContract (subst f y y' @@ Var y') (subst e2 y y')

        (Var x,[_and_ty,e1,e2])
            | isContrAnd x -> And <$> mkContract f e1 <*> mkContract f e2

        (Lam x e',[]) | isTyVar x -> do
            write $ "Skipping lambda of a type variable"
            mkContract f e'

        t -> throw $ "Found weird expression " ++ showExpr e ++
                     "\n  when making a contract for " ++ showExpr f ++
                     "\n  current trimmedTyApp was" ++ showOutputable t

getUnique' :: CollectM Unique
getUnique' = lift $ lift $ lift $ getUniqueM

-- | Updates the unique in a Var and Type: i.e. make it different from
--   the one we had, but otherwise identical.
refresh :: Var -> Type -> CollectM Var
refresh v ty = do
    u <- getUnique'
    return (setVarType (setVarUnique v u) ty)

mkFreshVar :: Type -> CollectM Var
mkFreshVar ty = do
    v <- gets head
    write $ "Making a fresh variable " ++ v ++ " with type " ++ showOutputable ty
    modify tail
    u <- getUnique'
    let name = mkInternalName u (mkOccName varName v) wiredInSrcSpan
    return (mkLocalId name ty)

write :: String -> CollectM ()
write = tell . return

throw :: String -> CollectM a
throw = throwError . strMsg
