module Contracts.Trans where

import CoreSyn
import TysWiredIn

import Contracts.Types

import Halt.PrimCon
import Halt.ExprTrans
import Halt.Common
import Halt.Monad


import Control.Monad.Reader

import Halt.AbstractFOL hiding (App,And,CF)

trStatement :: Statement -> HaltM [VarClause]
trStatement (Statement n v c) =
    sequence [Clause NegatedConjecture (show n) <$> trNeg (Var v) c
             -- ,FDecl Axiom (show n) <$> trPos (Var v) c
             ]

trPos :: CoreExpr -> Contract -> HaltM VarFormula
trPos e c = case c of
    Pred p -> do
        p_tr <- trExpr p
        return $ min' p_tr ==> (p_tr === fun0 trueDataConId
                             \/ p_tr === constant UNR)
    CF -> do
        e_tr <- trExpr e
        return $ min' e_tr ==> cf e_tr
    And c1 c2 -> (/\) <$> trPos e c1 <*> trPos e c2
    Arrow v c1 c2 -> local (pushQuant [v]) $ do
         fx <- trExpr (e `App` Var v)
         l <- trNeg (Var v) c1
         r <- trPos (e `App` Var v) c2
         return $ forall' [v] (min' fx ==> (l \/ r))

trNeg :: CoreExpr -> Contract -> HaltM VarFormula
trNeg e c = case c of
    Pred p -> do
        p_tr <- trExpr p
        return $ min' p_tr /\ (p_tr === fun0 falseDataConId
                            \/ p_tr === constant BAD)
    CF -> do
        e_tr <- trExpr e
        return $ min' e_tr /\ Neg (cf e_tr)
    And c1 c2 -> (\/) <$> trNeg e c1 <*> trNeg e c2
    Arrow v c1 c2 -> local (pushQuant [v]) $ do
         l <- trPos (Var v) c1
         r <- trNeg (e `App` Var v) c2
         return $ exists' [v] (l /\ r)



