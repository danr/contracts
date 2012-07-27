{-# LANGUAGE RecordWildCards #-}
{-

    Translates contracts in the datatypes in Contracts.Types to FOL

-}
module Contracts.Trans where

import CoreSyn
import Var

import Contracts.Types
import Contracts.FixpointInduction
import Contracts.Params
import Contracts.Theory

import Halo.ExprTrans
import Halo.Util
import Halo.Monad
import Halo.Subtheory
import Halo.Shared
import Halo.PrimCon
import Halo.FOL.Abstract

import Control.Monad.Reader

import qualified Data.Map as M

-- | We want to access the params and the fix info while doing this
type TransM = ReaderT TrEnv HaloM

data TrEnv = TrEnv
    { env_params   :: Params
    , env_fix_info :: FixInfo
    , env_bind_map :: HCCBinds
    }

getParams :: TransM Params
getParams = asks env_params

getFixInfo :: TransM FixInfo
getFixInfo = asks env_fix_info

getBindPart :: Var -> TransM [HCCBindPart]
getBindPart x = asks ((M.! x) . env_bind_map)

data Skolem = Skolemise | Quantify
  deriving (Eq,Ord,Show)

trTopStmt :: TopStmt -> TransM [Conjecture]
trTopStmt (TopStmt _name stmt deps) =
    trStatement deps (stripTreeUsings stmt)

trStatement :: [HCCContent] -> Statement -> TransM [Conjecture]
trStatement deps stmt@Statement{..} = do

    Params{..} <- getParams

    fpi_content <- trFPI deps stmt
    plain_content <- trPlain deps (trContract Neg) Skolemise stmt

    let conjectures
            | fpi_no_plain && not (null fpi_content) = fpi_content
            | otherwise = plain_content : fpi_content

    (using_clauses,using_deps) <- (`mapAndUnzipM` statement_using) $ \used_stm ->
        local' (addSkolems statement_args) $ do
            Conjecture u_cl u_dep _ <- trPlain deps (trContract Pos) Quantify used_stm
            return $ (u_cl,u_dep)

    let extender = extendConj (concat using_clauses) (concat using_deps)

    return $ map extender conjectures

trPlain :: [HCCContent] -> (Skolem -> CoreExpr -> Contract -> TransM Formula')
        -> Skolem -> Statement -> TransM Conjecture
trPlain deps tr_fun sk stmt@(Statement e c as _) = do

    (tr_contr,ptrs) <- capturePtrs' $ do
        let before = case sk of
                Skolemise -> local' (addSkolems as)
                Quantify  -> fmap (forall' as)
        before (tr_fun sk e c)

    let cltype = case sk of
            Skolemise -> negatedConjecture
            Quantify  -> hypothesis

        clauses = comment (show stmt) : clauseSplit cltype tr_contr

        content_dep = map Pointer ptrs ++ deps

    return $ Conjecture
        { conj_clauses      = clauses
        , conj_dependencies = content_dep
        , conj_kind         = Plain
        }

-- | The top variable, suitable for fixed point induction
topVar :: CoreExpr -> Maybe Var
topVar (Var v)    = Just v
topVar (App e _)  = topVar e
topVar (Lam _ e)  = topVar e
topVar (Cast e _) = topVar e
topVar (Tick _ e) = topVar e
topVar _          = Nothing

trFPI :: [HCCContent] -> Statement -> TransM [Conjecture]
trFPI deps (Statement e c as _) = do
    fix_info <- getFixInfo
    Params{..} <- getParams
    case topVar e of
        Just f | fpiApplicable fix_info f -> local' (addSkolems as) $ do

            let [f_base,f_hyp,f_concl]
                    = map (fpiFocusName fix_info f) [ConstantUNR,Hyp,Concl]

                -- Change dependencies from f to f_base or f_concl
                rename_f Base v | v == f = f_base
                rename_f Step v | v == f = f_concl
                rename_f _    v = v

                [deps_base,deps_step]
                    = [ map (mapFunctionContent
                             ( fpiFriendName fix_info f friend_case
                             . rename_f friend_case
                             )) deps
                      | friend_case <- [Base,Step]
                      ]

                rename_c = substContractList c . fpiGetSubstList fix_info f

            let e_subst = subst e f

            (tr_base,ptrs_base) <- capturePtrs' $
                clauseSplit axiom <$>
                trContract Neg Skolemise (e_subst f_base) (rename_c ConstantUNR)

            (tr_step,ptrs_step) <- capturePtrs' $ do
                hyp   <- clauseSplit hypothesis <$>
                            trContract Pos Quantify (e_subst f_hyp) (rename_c Hyp)
                concl <- clauseSplit axiom <$>
                            trContract Neg Skolemise (e_subst f_concl) (rename_c Concl)
                return $ hyp ++ concl

            let content_base = deps_base ++ map Pointer ptrs_base
                content_step = deps_step ++ map Pointer ptrs_step

            return $
                [ Conjecture
                    { conj_clauses      = tr_base
                    , conj_dependencies = content_base
                    , conj_kind         = FixpointBase
                    }
                | not fpi_no_base ] ++
                [ Conjecture
                    { conj_clauses      = tr_step
                    , conj_dependencies = content_step
                    , conj_kind         = FixpointStep
                    }
                ]

        _ -> return []

data Variance = Pos | Neg deriving (Eq,Show)

opposite :: Variance -> Variance
opposite Pos = Neg
opposite Neg = Pos

trContract :: Variance -> Skolem -> CoreExpr -> Contract -> TransM Formula'
trContract variance skolemise e_init contract = do

    let (arguments,result) = telescope contract
        vars     = map fst arguments
        e_result = foldl (@@) e_init (map Var vars)

    lift $ write $ "trContract (" ++ show skolemise ++ ")" ++ " " ++ show variance
                ++ "\n    e_init    :" ++ showExpr e_init
                ++ "\n    e_result  :" ++ showExpr e_result
                ++ "\n    contract  :" ++ show contract
                ++ "\n    result    :" ++ show result
                ++ "\n    arguments :" ++ show arguments
                ++ "\n    vars      :" ++ show vars

    tr_contract <- local' (skolemise == Skolemise ? addSkolems vars) $ do

        lift $ write $ "Translating arguments of " ++ showExpr e_result

        let tr_argument :: (Var,Contract) -> TransM Formula'
            tr_argument = uncurry (trContract (opposite variance) Quantify . Var)

        tr_arguments <- mapM tr_argument arguments

        lift $ write $ "Translating result of " ++ showExpr e_result

        tr_result <- case result of

            Pred p -> do
                ex <- lift $ trExpr e_result
                px <- lift $ trExpr p
                return $ case variance of
                    Neg -> min' ex /\ min' px /\ ex =/= unr /\ (px === false \/ px === bad)
                    Pos -> min' ex ==> (min' px /\ (ex === unr \/ px === unr \/ px === true))

            CF -> do
                e_tr <- lift $ trExpr e_result
                return $ case variance of
                    Neg -> min' e_tr /\ neg (cf e_tr)
                    Pos -> min' e_tr ==> cf e_tr

            And c1 c2 -> case variance of { Neg -> ors ; Pos -> ands }
                <$> mapM (trContract variance skolemise e_result) [c1,c2]

            Arrow{} -> error "trContract : telescope didn't catch arrow"

        return $ tr_arguments ++ [tr_result]

    return $ case variance of
        Neg -> (skolemise == Quantify ? exists' vars) (ands tr_contract)
        Pos -> forall' vars (ors tr_contract)

local' :: (HaloEnv -> HaloEnv) -> TransM a -> TransM a
local' k m = do
    e <- ask
    lift $ (local k) (runReaderT m e)

capturePtrs' :: TransM a -> TransM (a,[Var])
capturePtrs' m = do
    e <- ask
    lift $ capturePtrs (runReaderT m e)
