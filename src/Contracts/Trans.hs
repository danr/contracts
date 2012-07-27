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

import Halo.Binds
import Halo.ExprTrans
import Halo.FOL.Abstract
import Halo.Monad
import Halo.PrimCon
import Halo.Shared
import Halo.Subtheory
import Halo.Util

import Control.Monad.Reader

import qualified Data.Map as M
import Data.List
import Data.Maybe

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

getBindParts :: Var -> TransM [HCCBindPart]
getBindParts x = asks (fromMaybe err . M.lookup x . env_bind_map)
  where
    err = error $ "Contracts.Trans.getBindParts: no bind parts for " ++ show x

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

                -- We use the original dependencies, but rename f to
                -- f_base or f_concl in dependencies
                [deps_base,deps_step]
                    = [ map (mapFunctionContent
                             ( fpiFriendName fix_info f friend_case
                             . rename_f friend_case
                             )) deps
                      | friend_case <- [Base,Step]
                      ]

                -- How to rename an entire contract
                rename_c = substContractList c . fpiGetSubstList fix_info f

            let e_subst = subst e f

            (tr_base,ptrs_base) <- capturePtrs' $
                clauseSplit axiom <$>
                trContract Neg Skolemise (e_subst f_base) (rename_c ConstantUNR)

            (tr_hyp,ptrs_hyp) <- capturePtrs' $
                clauseSplit hypothesis <$>
                trContract Pos Quantify (e_subst f_hyp) (rename_c Hyp)

            (tr_concl,ptrs_concl) <- capturePtrs' $
                clauseSplit hypothesis <$>
                trContract Neg Skolemise (e_subst f_concl) (rename_c Concl)

            let content_base  = map Pointer ptrs_base ++ deps_base
                content_hyp   = map Pointer ptrs_hyp
                content_concl = map Pointer ptrs_concl ++ deps_step

            splits <- trSplit (e_subst f_concl) (rename_c Concl)

            return $
                [ Conjecture
                    { conj_clauses      = tr_base
                    , conj_dependencies = content_base
                    , conj_kind         = FixpointBase
                    }
                | not fpi_no_base ] ++
                [ Conjecture
                    { conj_clauses      = tr_hyp ++ tr_concl
                    , conj_dependencies = nub (content_hyp ++ content_concl)
                    , conj_kind         = FixpointStep
                    }
                ] ++
                [ Conjecture
                    { conj_clauses      = tr_hyp ++ split_clauses
                    , conj_dependencies = nub (delete (Function f_concl) content_concl ++ split_deps)
                    , conj_kind         = FixpointStepSplit split_num
                    }
                | Split{..} <- splits
                ]


        _ -> return []

data Variance = Pos | Neg deriving (Eq,Show)

opposite :: Variance -> Variance
opposite Pos = Neg
opposite Neg = Pos

data Split = Split
    { split_clauses :: [Clause']
    , split_deps    :: [HCCContent]
    , split_num     :: Int
    }

trSplit :: CoreExpr -> Contract -> TransM [Split]
trSplit expr contract = do
    let f = fromMaybe (error "trSplit: topVar returned Nothing") (topVar expr)

    bind_parts <- getBindParts f

    let (min_parts,decl_parts) = partition (minRhs . bind_rhs) bind_parts

    -- Translate everything about min for now
    (tr_min,min_ptrs) <- lift $
        ((comment ("Axioms about min for " ++ show f):) . axioms *** pointers)
        <$> capturePtrs (mapM trBindPart min_parts)

    let min_deps = concatMap bind_deps min_parts ++ min_ptrs

    (tr_contr,contr_deps) <-
        ((comment "Contract" :) . axioms . splitFormula *** pointers)
        <$> capturePtrs' (trContract Neg Skolemise expr contract)

    forM (zip decl_parts [0..]) $ \ (decl_part@BindPart{..},num) -> do

        (tr_part,part_ptrs) <- lift $ capturePtrs $ do

            -- Translate just this bind part
            tr_decl <- ((comment "Bind part":) . definitions . splitFormula) <$>
                trBindPart decl_part

            -- Foreach argument e, match up the variable v
            -- introduce e's arguments as skolems and make them equal

            let vars,sks :: [Var]
                vars = map fst . fst . telescope $ contract
                sks = concatMap exprFVs (bind_args)

            tr_goal <- local (addSkolems (nub $ sks ++ vars)) $ do
                tr_eqs <- axioms .: zipWith (===)
                    <$> mapM (trExpr . Var) vars
                    <*> mapM trExpr bind_args
                tr_constrs <- axioms <$> trConstraints bind_constrs
                return $ [comment "Equalities from arguments"] ++ tr_eqs
                      ++ [comment "Imposed constraints"] ++ tr_constrs

            return (tr_decl ++ tr_goal)

        let deps = delete (Function f) $
                min_deps ++ contr_deps ++ bind_deps ++ map Pointer part_ptrs

            clauses = tr_min ++ tr_part ++ tr_contr

        return $ Split
            { split_clauses = clauses
            , split_deps    = deps
            , split_num     = num
            }

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
