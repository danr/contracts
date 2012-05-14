module Contracts.Trans where

import CoreSyn
import Var
import TysWiredIn

import Contracts.Types

import Halt.Names
import Halt.ExprTrans
import Halt.Common
import Halt.Monad

import Control.Monad.Reader

import FOL.Abstract hiding (App,And,CF)

trStatement :: Statement -> HaltM [Clause Var]
trStatement (Statement n v c) =
    sequence [Clause NegatedConjecture (show n) <$> trNeg (Var v) c
             -- ,FDecl Axiom (show n) <$> trPos (Var v) c
             ]

trPos :: CoreExpr -> Contract -> HaltM (Formula Var)
trPos e c = case c of
    Pred p -> do
        p_tr <- trExpr p
        return $ minPred p_tr ==> (p_tr === con trueDataConId
                                \/ p_tr === con (constantId UNR))
    CF -> do
        e_tr <- trExpr e
        return $ minPred e_tr ==> cfPred e_tr
    And c1 c2 -> (/\) <$> trPos e c1 <*> trPos e c2
    Arrow v c1 c2 -> local (pushQuant [v]) $ do
         fx <- trExpr (e `App` Var v)
         l <- trNeg (Var v) c1
         r <- trPos (e `App` Var v) c2
         return $ forall' [v] (minPred fx ==> (l \/ r))

trNeg :: CoreExpr -> Contract -> HaltM (Formula Var)
trNeg e c = case c of
    Pred p -> do
        p_tr <- trExpr p
        return $ minPred p_tr /\ (p_tr === con falseDataConId
                               \/ p_tr === con (constantId BAD))
    CF -> do
        e_tr <- trExpr e
        return $ minPred e_tr /\ Neg (cfPred e_tr)
    And c1 c2 -> (\/) <$> trNeg e c1 <*> trNeg e c2
    Arrow v c1 c2 -> local (pushQuant [v]) $ do
         l <- trPos (Var v) c1
         r <- trNeg (e `App` Var v) c2
         return $ exists' [v] (l /\ r)



