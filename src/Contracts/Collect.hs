{-# LANGUAGE TupleSections #-}
-- This module collects the contracts in the source file and turns
-- them into hcc's internal reperesantion of contracts,
-- i.e. translating the higher order abstract syntax that is used.

module Contracts.Collect where

import Var hiding (varName)
import Name
import SrcLoc
import Id
import CoreSyn
import Outputable
import CoreSubst
import UniqSupply
import CoreFVs
import UniqSet

import Contracts.SrcRep
import Contracts.Types

import Halo.Util
import Halo.Shared

import Data.Maybe
import Data.Either

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader

import Data.List

collectContracts
    :: [CoreBind]
    -> UniqSM (Either String ([Statement],[CoreBind],[String]))
collectContracts program = runErrorT $ do
    let binds :: [(Var,CoreExpr)]
        binds = flattenBinds program

        statements :: [(Var,CoreExpr)]
        statements = filter (isStatementType . fst) binds

        program' :: [CoreBind]
        untranslated_stmts :: [(Var,CoreExpr)]
        (program',untranslated_stmts) = partitionEithers $ map pick program
          where
            pick :: CoreBind -> Either CoreBind (Var,CoreExpr)
            pick (NonRec v e) | isStatementType v = Right (v,e)
            pick b                                = Left b

    (stmts,([],db_msgs)) <- runCollectM binds $ do
        write "Untranslated statements:"
        write (unlines (map (show . fst) untranslated_stmts))
        write "Translating them..."
        let mkStatement' (v,e) = do
                (stmt,(deps,msgs)) <-
                    censor (\(deps,msgs) -> (mempty,msgs))
                           (listen (mkStatement v e))
                return $ stmt { statement_deps = statement_deps stmt ++ deps }
        mapM mkStatement' untranslated_stmts


    return (stmts,program',db_msgs)

type CollectM =
    (WriterT ([Var],[String])
    -- ^ Keeping track of used variables when translating contracts, and
    -- ^ writing debug messages
    (ReaderT [(Var,CoreExpr)]
    -- ^ The set of definitions from the start
    (StateT [String]
    -- ^ State of variable names
    (ErrorT String
    -- ^ We can error by throwing a string message
    UniqSM))))
    -- ^ Making variable names

runCollectM :: [(Var,CoreExpr)]
            -> CollectM a -> ErrorT String UniqSM (a,([Var],[String]))
runCollectM cbs m = (runWriterT m `runReaderT` cbs) `evalStateT` names
  where
    names = map ("ct_" ++) $ (`replicateM` ['a'..'z']) =<< [1..]

trimTyApp :: CoreExpr -> Maybe (CoreExpr,[CoreExpr])
trimTyApp e@App{} = Just (second trimTyArgs (collectArgs e))
trimTyApp e@Var{} = Just (e,[])
trimTyApp _       = Nothing

mkStatement :: Var -> CoreExpr -> CollectM Statement
mkStatement v e = do
    write $ "Translating statement " ++ show v ++ " = " ++ showExpr e
    case trimTyApp e of
        Just (Var x,[f_app,c]) | isStatementCon x -> do
            write $ "A contract for: " ++ showExpr f_app ++ "."
            f <- case trimTyApp f_app of
                Just (Var f,[]) -> do
                    write $ "Contract is really for " ++ show f ++ "."
                    return f
                _ -> throw $ "Invalid lhs of statement" ++ showExpr f_app
            contr <- mkContract (Var f) c
            return $ Statement v f contr [] [f | isLocalVar f ]
        Just (Var x,[stmnt,u]) | isStatementUsing x -> do
            write $ "A contract using: " ++ showExpr u
            f <- case trimTyApp u of
                Just (Var f,[]) -> do
                    write $ "Contract is really using " ++ show f ++ "."
                    return f
                _ -> throw $ "Invalid lhs of using " ++ showExpr u
            statement <- mkStatement v stmnt
            return $ statement { statement_using = f : statement_using statement }
        _ -> throw $ "Error: Invalid statement " ++ show v ++ "."

mkContract :: CoreExpr -> CoreExpr -> CollectM Contract
mkContract f e = do
    write $ showExpr f ++ ": " ++ showExpr e
    case trimTyApp e of
        Just (Var x,[])  | isContrCF x -> return CF
        Just (Var x,[lam@(Lam y p)])
            | isContrPred x -> do
                registerFreeVars f lam
                let s = extendIdSubst emptySubst y f
                    p' = substExpr (text "mkContract Pred") s p
                return (Pred p')
        Just (Var x,[e1,Lam y e2])
           | isContrPi x -> Arrow y <$> mkContract (Var y) e1
                                    <*> mkContract (f `App` Var y) e2
        Just (Var x,[p]) | isContrPred x
            -> registerFreeVars f p >> return (Pred (p `App` f))
        Just (Var x,[e1,e2])
           | isContrArr x -> do v <- mkFreshVar
                                Arrow v <$> mkContract (Var v) e1
                                        <*> mkContract (f `App` Var v) e2
          | isContrAnd x -> And <$> mkContract f e1 <*> mkContract f e2
        Just (Var x,[]) -> mkContract f =<< lookupBind x
        _ -> throw $ "Found weird expression " ++ showExpr e ++
                     " when making a contract for " ++ showExpr f

lookupBind :: Var -> CollectM CoreExpr
lookupBind x = do
    binds <- ask
    case find ((x ==) . fst) binds of
        Just (_,e) -> return e
        _ -> throw $ "Found " ++ show x ++ " not leading to anything in making of a contract"

mkFreshVar :: CollectM Var
mkFreshVar = do
    v <- gets head
    modify tail
    u <- lift $ lift $ lift $ lift $ getUniqueM
    let name = mkInternalName u (mkOccName varName v) wiredInSrcSpan
    return (mkVanillaGlobal name ty_err)
  where ty_err = error "mkFreshVar type"

register :: Var -> CollectM ()
register = registerMany . (:[])

registerMany :: [Var] -> CollectM ()
registerMany = tell . (,mempty)

registerFreeVars :: CoreExpr -> CoreExpr -> CollectM ()
registerFreeVars now new
    = registerMany
        (uniqSetToList (exprFreeVars new `minusUniqSet` exprFreeVars now))

write :: String -> CollectM ()
write = tell . (mempty,) . (:[])

rewrite :: [String] -> CollectM ()
rewrite = tell . (mempty,)

throw :: String -> CollectM a
throw = throwError . strMsg

