module Contracts.Trans where

import CoreSyn
import TysWiredIn
import Var

import Contracts.Types

import Halo.PrimCon
import Halo.ExprTrans
import Halo.Util
import Halo.Monad
import Halo.Subtheory

import Halo.FOL.Abstract

import Control.Monad.Reader

trStatement :: Statement -> HaloM ([Clause'],[Content])
trStatement stm@(Statement n v c deps) = do
    (tr_contr,ptrs) <- capturePtrs $ trNeg (Var v) c

    let clause = namedClause (show n) NegatedConjecture tr_contr

    return $
        ([comment (show stm),clause]
        ,map Function deps ++ map Pointer ptrs)

trPos :: CoreExpr -> Contract -> HaloM Formula'
trPos e c = case c of
    Pred p -> do
        p_tr <- trExpr p
        return $ min' p_tr ==> (p_tr === con trueDataConId
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

trNeg :: CoreExpr -> Contract -> HaloM Formula'
trNeg e c = case c of
    Pred p -> do
        p_tr <- trExpr p
        return $ min' p_tr /\ (p_tr === con falseDataConId
                            \/ p_tr === constant BAD)
    CF -> do
        e_tr <- trExpr e
        return $ min' e_tr /\ neg (cf e_tr)
    And c1 c2 -> (\/) <$> trNeg e c1 <*> trNeg e c2
    Arrow v c1 c2 -> local (pushQuant [v]) $ do
         l <- trPos (Var v) c1
         r <- trNeg (e `App` Var v) c2
         return $ exists' [v] (l /\ r)



