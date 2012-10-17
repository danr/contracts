{-# LANGUAGE PatternGuards #-}
{-

    This module collects the contracts in the source file and turns
    them into hcc's internal reperesantion of contracts,
    i.e. translating the higher order abstract syntax that is used.

    The inliner must be run before, otherwise this will throw errors

-}
module Contracts.Internalise where

import CoreSyn
import Id
import Name
import SrcLoc
import Type
import UniqSupply
import Unique
import Var hiding (varName)

import Contracts.SourceRepr
import Contracts.InternalRepr

import Halo.Util
import Halo.Shared

import Data.Either

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.State

internaliseContracts :: [CoreBind]
                 -> UniqSM (Either String ([TopStmt],[CoreBind],[String]))
internaliseContracts program = runErrorT $ do
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

    (stmts,db_msgs) <- runInternaliseM $ do
        write "Internalising statements:"
        write (unlines (map (showOutputable . fst) untranslated_stmts))
        mapM (uncurry mkTopStmt) untranslated_stmts

    return (stmts,program',db_msgs)

type InternaliseM =
    (WriterT [String]
    -- ^ Writing debug messages
    (StateT [String]
    -- ^ State of variable names
    (ErrorT String
    -- ^ We can error by throwing a string message
    UniqSM)))
    -- ^ Making variable names

-- | Run the monad
--
--   TODO: Would rather want to get the debug messages together with
--   the error message.
runInternaliseM :: InternaliseM a -> ErrorT String UniqSM (a,[String])
runInternaliseM m = runWriterT m `evalStateT` names
  where
    names = (`replicateM` ['a'..'z']) =<< [1..]

trimTyApp :: CoreExpr -> Maybe (CoreExpr,[CoreExpr])
trimTyApp e@App{} = Just (second trimTyArgs (collectArgs e))
trimTyApp e@Var{} = Just (e,[])
trimTyApp e@Lam{} = Just (e,[])
trimTyApp _       = Nothing

mkTopStmt :: Var -> CoreExpr -> InternaliseM TopStmt
mkTopStmt name e = do
    write $ "Making a top statement for " ++ showOutputable name
    stmt <- unTreeStmt <$> mkStatement e
    return $ TopStmt
        { top_name = name
        , top_stmt = stmt
        , top_deps = stmtDeps stmt
        }

mkStatement :: CoreExpr -> InternaliseM Statement
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
                write $ "Binding " ++ showOutputable y ++ " in a statement " ++ showExpr s
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

mkContract :: CoreExpr -> CoreExpr -> InternaliseM Contract
mkContract f e = do
    write $ showExpr f ++ ": " ++ showExpr e

    let mkPi c1_ty e1 y e2 = do
            y' <- refresh y c1_ty
            Arrow y' <$> mkContract (Var y') (subst y y' e1)
                     <*> mkContract (subst y y' f @@ Var y') (subst y y' e2)

    case collectArgs e of
        (Var x,[_cf_ty])     | isContrCF x -> return CF
        (Var x,[_pred_ty,p]) | isContrPred x -> return (Pred (p @@ f))

        -- I don't know why both these appear
        (Var x,[Type c1_ty,Type _c2_ty,e1,Lam y e2])
            | isContrPi x -> mkPi c1_ty e1 y e2

        (Var x,[Type _c_ty,Type c1_ty,Type _c2_ty,Coercion _co,e1,Lam y e2])
            | isContrPi x ->  mkPi c1_ty e1 y e2

        (Var x,[_and_ty,e1,e2])
            | isContrAnd x -> And <$> mkContract f e1 <*> mkContract f e2

        (Lam x e',[]) | isTyVar x -> do
            write $ "Skipping lambda of a type variable"
            mkContract f e'

        t -> throw $ "Found weird expression " ++ showExpr e ++
                     "\n  when making a contract for " ++ showExpr f ++
                     "\n  current trimmedTyApp was" ++ showOutputable t

-- | Get a Unique from this monad
getUnique' :: InternaliseM Unique
getUnique' = lift $ lift $ lift $ getUniqueM

-- | Updates the unique in a Var and Type: i.e. make it different from
--   the one we had, but otherwise identical.
refresh :: Var -> Type -> InternaliseM Var
refresh v ty = do
    u <- getUnique'
    return (setVarType (setVarUnique v u) ty)

-- | Makes a local variable witha given type
mkFreshVar :: Type -> InternaliseM Var
mkFreshVar ty = do
    v <- gets head
    write $ "Making a fresh variable " ++ v ++ " with type " ++ showOutputable ty
    modify tail
    u <- getUnique'
    let name = mkInternalName u (mkOccName varName v) wiredInSrcSpan
    return (mkLocalId name ty)

-- | Write a debug message
write :: String -> InternaliseM ()
write = tell . return

-- | Throw an error
throw :: String -> InternaliseM a
throw = throwError . strMsg
