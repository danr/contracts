module Contracts.Trans where

import CoreSyn

import Contracts.Types

import Halt.Names
import Halt.ExprTrans

import Halt.Utils
import Halt.Common
import Halt.Monad

import Control.Monad.Reader

import FOL.Syn

trStatement :: Statement -> HaltM FDecl
trStatement (Statement n v c) = FDecl NegConj (show n) <$> trNeg (Var v) c

trPos :: CoreExpr -> Contract -> HaltM Formula
trPos e c = case c of
    Pred p -> do
        p_tr <- trExpr p
        return $ minPred p_tr ==> (p_tr === mkConst "true"
                                \/ p_tr === mkConst (show UNR))
    CF -> do
        e_tr <- trExpr e
        return $ minPred e_tr ==> mkCF e_tr
    And c1 c2 -> (/\) <$> trPos e c1 <*> trPos e c2
    Arrow v c1 c2 -> local (pushQuant [v]) $ do
         fx <- trExpr (e `App` Var v)
         l <- trNeg (Var v) c1
         r <- trPos (e `App` Var v) c2
         return $ forall' [ mkVarName v ] (minPred fx ==> (l \/ r))

trNeg :: CoreExpr -> Contract -> HaltM Formula
trNeg e c = case c of
    Pred p -> do
        p_tr <- trExpr p
        return $ minPred p_tr /\ (p_tr === mkConst "false"
                               \/ p_tr === mkConst (show BAD))
    CF -> do
        e_tr <- trExpr e
        return $ minPred e_tr /\ Neg (mkCF e_tr)
    And c1 c2 -> (\/) <$> trNeg e c1 <*> trNeg e c2
    Arrow v c1 c2 -> local (pushQuant [v]) $ do
         l <- trPos (Var v) c1
         r <- trNeg (e `App` Var v) c2
         return $ exists' [ mkVarName v ] (l /\ r)



