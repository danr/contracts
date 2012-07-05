module Contracts.Trans where

import CoreSyn
import Var

import Contracts.Types
import Contracts.FixpointInduction
import Contracts.Theory

import Halo.PrimCon
import Halo.ExprTrans
import Halo.Util
import Halo.Monad
import Halo.Subtheory
import Halo.Data (true,false,unr,bad)
import Halo.FOL.Abstract

import Control.Monad.Reader

import Data.Map (singleton)
import Data.List

type ProofContent = ([Clause'],[HCCContent])

findStatement :: [Statement] -> Var -> Statement
findStatement ss c = case find ((c ==) . statement_name) ss of
    Just s -> s
    Nothing -> error $ "Cannot find used assumption contract " ++ show c

trStatement :: [Statement] -> FixInfo -> Statement -> HaloM [(ProofPart,ProofContent)]
trStatement ss fix_info stm = do
    parts_and_content <- (:) <$> trPlain (trNeg True) stm <*> trFPI fix_info stm
    let used_statements = map (findStatement ss) (statement_using stm)
    (using,deps) <- flip mapAndUnzipM used_statements $ \used_stm -> do
         (_,content) <- trPlain trPos used_stm
         return $ content

    let extend (part,(clauses,contents)) =
          (part,(clauses ++ concat using,contents ++ concat deps))

    return $ map extend parts_and_content

trFPI :: FixInfo -> Statement -> HaloM [(ProofPart,ProofContent)]
trFPI fix_info stm@(Statement n f c _using deps)
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
            trNeg True (Var f_base) (rename_c ConstantUNR)

        (tr_step,ptrs_step) <- capturePtrs $ do
            hyp   <- clause Hypothesis <$>
                        trPos (Var f_hyp) (rename_c Hyp)
            concl <- clause NegatedConjecture <$>
                        trNeg True (Var f_concl) (rename_c Concl)
            return [hyp,concl]

        let content_base = map Function deps_base ++ map Pointer ptrs_base
            content_step = map Function deps_step ++ map Pointer ptrs_step

        return
            [(FixpointBase,(tr_base,content_base))
            ,(FixpointStep,(tr_step,content_step))]

    | otherwise = return []

trPlain :: (CoreExpr -> Contract -> HaloM Formula')
        -> Statement -> HaloM (ProofPart,ProofContent)
trPlain tr_fun stm@(Statement n v c _ deps) = do
    (tr_contr,ptrs) <- capturePtrs $ tr_fun (Var v) c

    let clauses =
            [comment (show stm)
            ,namedClause (show n) NegatedConjecture tr_contr]

        content_dep = map Function deps ++ map Pointer ptrs

    return (Plain,(clauses,content_dep))

trPos :: CoreExpr -> Contract -> HaloM Formula'
trPos e c = case c of
    Pred p -> do
        x  <- trExpr e
        px <- trExpr p
        return $ min' x ==> (min' px /\ (x === unr \/ px === unr \/ px === true))
    CF -> do
        e_tr <- trExpr e
        return $ min' e_tr ==> cf e_tr
    And c1 c2 -> (/\) <$> trPos e c1 <*> trPos e c2
    Arrow v c1 c2 -> local (pushQuant [v]) $ do
        fx <- trExpr (e `App` Var v)
        l <- trNeg False (Var v) c1
        r <- trPos (e `App` Var v) c2
        return $ forall' [v] (l \/ r)

trNeg :: Bool -> CoreExpr -> Contract -> HaloM Formula'
trNeg skolemise e c = case c of
    Pred p -> do
        x  <- trExpr e
        px <- trExpr p
        return $ min' x /\ min' px /\ x =/= unr /\ (px === false \/ px === bad)
          -- Make it a flag to say px =/= True /\ px =/= unr just for experimentation and our own understanding.

    CF -> do
        e_tr <- trExpr e
        return $ min' e_tr /\ neg (cf e_tr)

    And c1 c2 -> (\/) <$> trNeg skolemise e c1 <*> trNeg skolemise e c2

    Arrow v c1 c2 -> local (if skolemise then addSkolem v else pushQuant [v]) $ do
        l <- trPos (Var v) c1
        r <- trNeg skolemise (e `App` Var v) c2
        return $ (not skolemise ? exists' [v]) (l /\ r)
