module Contracts.Trans where

import CoreSyn
import TysWiredIn
import Var

import Contracts.Types
import Contracts.FixpointInduction

import Halo.PrimCon
import Halo.ExprTrans
import Halo.Util
import Halo.Monad
import Halo.Subtheory
import Halo.FOL.Abstract

import Control.Monad.Reader

import Data.Map (singleton)

type ProofContent = ([Clause'],[Content])

trStatement :: FixInfo -> Statement -> HaloM [(ProofPart,ProofContent)]
trStatement fix_info stm = (:) <$> trPlain stm <*> trFPI fix_info stm

trFPI :: FixInfo -> Statement -> HaloM [(ProofPart,ProofContent)]
trFPI fix_info stm@(Statement n f c deps)
    | fpiApplicable fix_info f = do

        let [f_base,f_hyp,f_concl]
                = map (fpiFocusName fix_info f) [ConstantUNR,Hyp,Concl]

            -- Change dependencies from f to f_base or f_concl
            rename_f Base v | v == f = f_base
            rename_f Step v | v == f = f_concl
            rename_f _    v = v

            [deps_base,deps_step]
                = [ map ( fpiFriendName fix_info f friend_case
                        . rename_f friend_case ) deps
                  | friend_case <- [Base,Step]
                  ]

            rename_c = substContractList c . fpiGetSubstList fix_info f

        (tr_base,ptrs_base) <- capturePtrs $
            return . clause NegatedConjecture <$>
            trNeg (Var f_base) (rename_c ConstantUNR)

        (tr_step,ptrs_step) <- capturePtrs $ do
            hyp   <- clause Hypothesis <$>
                        trPos (Var f_hyp) (rename_c Hyp)
            concl <- clause NegatedConjecture <$>
                        trNeg (Var f_concl) (rename_c Concl)
            return [hyp,concl]

        let content_base = map Function deps_base ++ map Pointer ptrs_base
            content_step = map Function deps_step ++ map Pointer ptrs_step

        return
            [(FixpointBase,(tr_base,content_base))
            ,(FixpointStep,(tr_step,content_step))]

    | otherwise = return []


trPlain :: Statement -> HaloM (ProofPart,ProofContent)
trPlain stm@(Statement n v c deps) = do
    (tr_contr,ptrs) <- capturePtrs $ trNeg (Var v) c

    let clauses =
            [comment (show stm)
            ,namedClause (show n) NegatedConjecture tr_contr]

        content_dep = map Function deps ++ map Pointer ptrs

    return (Plain,(clauses,content_dep))

trPos :: CoreExpr -> Contract -> HaloM Formula'
trPos e c = case c of
    Pred p -> do
        x    <- trExpr e
        p_tr <- trExpr p
        return $ ors
            [p_tr === con trueDataConId
            ,p_tr === constant UNR
            -- ,x    === constant UNR
            -- do we need this?
            ]
    CF -> do
        e_tr <- trExpr e
        return $ cf e_tr
    And c1 c2 -> (/\) <$> trPos e c1 <*> trPos e c2
    Arrow v c1 c2 -> local (pushQuant [v]) $ do
        fx <- trExpr (e `App` Var v)
        l <- trNeg (Var v) c1
        r <- trPos (e `App` Var v) c2
        return $ forall' [v] (l \/ r)

trNeg :: CoreExpr -> Contract -> HaloM Formula'
trNeg e c = case c of
    Pred p -> do
        x <- trExpr e
        p_tr <- trExpr p
        return $ ors
            [p_tr === con falseDataConId
            ,p_tr === constant BAD
            -- ,x    === constant UNR
            -- do we need this?
            ]
    CF -> do
        e_tr <- trExpr e
        return $ neg (cf e_tr)
    And c1 c2 -> (\/) <$> trNeg e c1 <*> trNeg e c2
    Arrow v c1 c2 -> local (pushQuant [v]) $ do
        l <- trPos (Var v) c1
        r <- trNeg (e `App` Var v) c2
        return $ exists' [v] (l /\ r)



