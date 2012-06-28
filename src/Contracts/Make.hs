-- This module makes contracts

module Contracts.Make where

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

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader

import Data.List


collectContracts :: UniqSupply -> [CoreBind] ->
                    ((Either String ([Statement],[CoreBind]),[String]),UniqSupply)
collectContracts us program = runMaker us binds $ do
    write "Statements:"
    write (unlines (map (show . fst) statements))
    write "Translating them..."
    contracts <- mapM (uncurry mkStatement) statements
    return (contracts,program')
  where
    binds :: [(Var,CoreExpr)]
    binds = flattenBinds program

    statements :: [(Var,CoreExpr)]
    statements = filter (isStatementType . fst) binds

    program' :: [CoreBind]
    program' = mapMaybe (\b -> case b of
                                  NonRec v _ | isStatementType v -> Nothing
                                  _                              -> Just b)
                        program

type MakerM a = ErrorT String 
                (WriterT [String] 
                 (StateT [String] 
                  (ReaderT [(Var,CoreExpr)] 
                   UniqSM))) a

runMaker :: UniqSupply -> [(Var,CoreExpr)] 
            -> MakerM a -> ((Either String a,[String]),UniqSupply)
runMaker us cbs m = initUs us ((runWriterT (runErrorT m) `evalStateT` names) `runReaderT` cbs)
  where
    names = map ("ct_" ++) $ (`replicateM` ['a'..'z']) =<< [1..]

lookupBind :: Var -> MakerM CoreExpr
lookupBind x = do
    binds <- ask
    case find ((x ==) . fst) binds of
        Just (_,e) -> return e
        _ -> throw $ "Found " ++ show x ++ " not leading to anything in making of a contract"

mkFreshVar :: MakerM Var
mkFreshVar = do
    v <- gets head
    modify tail
    u <- lift $ lift $ lift $ lift $ getUniqueM
    let name = mkInternalName u (mkOccName varName v) wiredInSrcSpan
    return (mkVanillaGlobal name ty_err)
  where ty_err = error "mkFreshVar type"


write :: String -> MakerM ()
write = tell . (:[])

throw :: String -> MakerM a
throw = throwError . strMsg

trimTyApp :: CoreExpr -> Maybe (CoreExpr,[CoreExpr])
trimTyApp e@App{} = Just (second trimTyArgs (collectArgs e))
trimTyApp e@Var{} = Just (e,[])
trimTyApp _       = Nothing

mkStatement :: Var -> CoreExpr -> MakerM Statement
mkStatement v e = do
    write $ "Translating statement " ++ show v ++ " = " ++ showExpr e
    case trimTyApp e of
        Just (Var x,[f_app,c]) | isStatementCon x -> do
            write $ "A contract for: " ++ showExpr f_app ++ "."
            f <- case trimTyApp f_app of
                Just (Var f,[]) -> do write $ "Contract is really for " ++ show f ++ "."
                                      return f
                _               -> throw $ "Invalid lhs of statement" ++ showExpr f_app
            contr <- mkContract (Var f) c
            let deps = uniqSetToList (exprFreeVars e)
            return $ Statement v f contr [] deps
        Just (Var x,[stmnt,u]) | isStatementUsing x -> do
            write $ "A contract using: " ++ showExpr u
            f <- case trimTyApp u of
                Just (Var f,[]) -> do write $ "Contract is really using " ++ show f ++ "."
                                      return f
                _               -> throw $ "Invalid lhs of using " ++ showExpr u
            statement <- mkStatement v stmnt            
            return $ statement { statement_using = f : statement_using statement }        
        _ -> throw $ "Error: Invalid statement " ++ show v ++ "."

mkContract :: CoreExpr -> CoreExpr -> MakerM Contract
mkContract f e = do
    write $ showExpr f ++ ": " ++ showExpr e
    case trimTyApp e of
        Just (Var x,[])  | isContrCF x -> return CF
        Just (Var x,[Lam y p])
           | isContrPred x -> do
                 let s = extendIdSubst emptySubst y f
                     p' = substExpr (text "mkContract Pred") s p
                 return (Pred p')
        Just (Var x,[e1,Lam y e2])
           | isContrPi x -> Arrow y <$> mkContract (Var y) e1
                                    <*> mkContract (f `App` Var y) e2
        Just (Var x,[p]) | isContrPred x -> return (Pred (p `App` f))
        Just (Var x,[e1,e2])
           | isContrArr x -> do v <- mkFreshVar
                                Arrow v <$> mkContract (Var v) e1
                                        <*> mkContract (f `App` Var v) e2
          | isContrAnd x -> And <$> mkContract f e1 <*> mkContract f e2
        Just (Var x,[]) -> mkContract f =<< lookupBind x
        _ -> throw $ "Found weird expression " ++ showExpr e ++ 
                     " when making a contract for " ++ showExpr f
 
