{-# LANGUAGE RecordWildCards #-}
module Contracts.Trans where

import CoreSyn
import Var

import Contracts.Types
import Contracts.FixpointInduction
import Contracts.Theory
import Contracts.Params

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

trStatement :: Params -> [Statement] -> FixInfo -> Statement -> HaloM [(ProofPart,ProofContent)]
trStatement params@Params{..} ss fix_info stm = do

    fpi_content <- trFPI params fix_info stm
    plain_content <- trPlain (trNeg params Goal Skolemise) stm

    let parts_and_content
            | fpi_no_plain && not (null fpi_content) = fpi_content
            | otherwise = plain_content : fpi_content

    let used_statements = map (findStatement ss) (statement_using stm)
    (using,deps) <- flip mapAndUnzipM used_statements $ \used_stm -> do
         (_,content) <- trPlain (trPos params Assumption) used_stm
         return $ content

    let extend (part,(clauses,contents)) =
          (part,(clauses ++ concat using,contents ++ concat deps))

    return $ map extend parts_and_content

trFPI :: Params -> FixInfo -> Statement -> HaloM [(ProofPart,ProofContent)]
trFPI params fix_info stm@(Statement n f c _using deps)
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
            trNeg params Goal Skolemise (Var f_base) (rename_c ConstantUNR)

        (tr_step,ptrs_step) <- capturePtrs $ do
            hyp   <- clause Hypothesis <$>
                        trPos params Assumption (Var f_hyp) (rename_c Hyp)
            concl <- clause NegatedConjecture <$>
                        trNeg params Goal Skolemise (Var f_concl) (rename_c Concl)
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

data Skolem = Skolemise | Quantify
  deriving (Eq,Ord,Show)

data Mode = Goal | Assumption
  deriving (Eq,Ord,Show)

trPos :: Params -> Mode -> CoreExpr -> Contract -> HaloM Formula'
trPos params@Params{..} mode e c = case c of
    Pred p -> do
        ex <- trExpr e
        px <- trExpr p
        return $ min' ex ==> (min' px /\ (ex === unr \/ px === unr \/ px === true))
    CF -> do
        e_tr <- trExpr e
        return $ min' e_tr ==> cf e_tr
    And c1 c2 -> (/\) <$> trPos params mode e c1 <*> trPos params mode e c2
    Arrow v c1 c2 -> local (pushQuant [v]) $ do
        fx <- trExpr (e `App` Var v)
        l <- trNeg params mode Quantify (Var v) c1
        r <- trPos params mode (e `App` Var v) c2
        return $ forall' [v] (l \/ r)

trNeg :: Params -> Mode -> Skolem -> CoreExpr -> Contract -> HaloM Formula'
trNeg params@Params{..} mode skolemise e c = case c of
    Pred p -> do
        ex <- trExpr e
        px <- trExpr p
        return $ min' ex /\ min' px /\ ex =/= unr /\ (px === false \/ px === bad)

    CF -> do
        e_tr <- trExpr e
        return $ min' e_tr /\ neg (cf e_tr)

    And c1 c2 -> (\/) <$> trNeg params mode skolemise e c1
                      <*> trNeg params mode skolemise e c2

    Arrow v c1 c2 -> local (case skolemise of
                                Skolemise -> addSkolem v
                                Quantify  -> pushQuant [v]) $ do
        l <- trPos params mode (Var v) c1
        r <- trNeg params mode skolemise (e `App` Var v) c2
        return $ (skolemise == Quantify ? exists' [v]) (l /\ r)
