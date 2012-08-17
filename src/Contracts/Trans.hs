{-# LANGUAGE RecordWildCards,NamedFieldPuns #-}
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
import Data.Maybe

data Variance = Pos | Neg deriving (Eq,Show)

opposite :: Variance -> Variance
opposite Pos = Neg
opposite Neg = Pos

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
trTopStmt (TopStmt _name stmt deps) = do

    Params{..} <- getParams

    fpi_content <- trFPI deps stmt
    plain_content <- trPlain deps stmt

    let conjectures
            | fpi_no_plain && not (null fpi_content) = fpi_content
            | otherwise = plain_content : fpi_content

    return conjectures

trPlain :: [HCCContent] -> Statement -> TransM Conjecture
trPlain deps stmt = do

    (clauses,ptr_deps) <- capturePtrs' (trStmt stmt)

    return $ Conjecture
        { conj_clauses      = comment "Plain contract":clauses
        , conj_dependencies = deps ++ ptr_deps
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

-- | Top variable of a statement, for fpi
topStmtVar :: Statement -> Maybe Var
topStmtVar (e ::: _)    = topVar e
topStmtVar (_ :=> t)    = topStmtVar t
topStmtVar (Lambda _ s) = topStmtVar s
topStmtVar (Using s _)  = topStmtVar s

-- | Try to translate this statement using FPI
trFPI :: [HCCContent] -> Statement -> TransM [Conjecture]
trFPI deps stmt = do
    fix_info <- getFixInfo
    case topStmtVar stmt of
        Just f | fpiApplicable fix_info f -> trFixated deps stmt f
        _ -> return []

-- | Translate this statement with this fixpoint function
trFixated :: [HCCContent] -> Statement -> Var -> TransM [Conjecture]
trFixated deps stmt f = do
    Params{..} <- getParams
    fix_info <- getFixInfo

    let -- Get the focused names
        [f_base,f_concl] = map (fpiFocusName fix_info f) [ConstantUNR,Concl]

        -- Change dependencies from f to f_base or f_concl
        rename_f Base v | v == f = f_base
        rename_f Step v | v == f = f_concl
        rename_f _    v = v

        -- We use the original dependencies, but rename f to
        -- f_base or f_concl in dependencies
        mk_deps :: FriendCase -> [HCCContent]
        mk_deps friend_case =
            map (mapFunctionContent ( fpiFriendName fix_info f friend_case
                                    . rename_f friend_case)) deps

        -- How to rename an entire statement
        rename_stmt = substStatementList stmt . fpiGetSubstList fix_info f

        -- Translate the contract for the base, hyp and conclusion focus,
        -- registering pointers and calculating the final dependencies
        mk_stmt :: FriendCase -> Statement
        mk_stmt Base = rename_stmt ConstantUNR
        mk_stmt Step = rename_stmt Hyp :=> rename_stmt Concl

        mk_fixpointcase Base = FixpointBase
        mk_fixpointcase Step = FixpointStep

    sequence
        [ do (tr,extra_deps) <- capturePtrs' (trStmt (mk_stmt cs))
             return $ Conjecture tr (mk_deps cs ++ extra_deps) (mk_fixpointcase cs)
        | cs <- [Base | not fpi_no_base] ++ [Step]
        ]

{- -- Splits are inactivated for now
    -- Also split the goal if possible
    splits <- trSplit (subst e f f_concl) (rename_c Concl)

        [ Conjecture tr_split deps_split (FixpointStepSplit split_num)
        | fpi_split
        , Split{..} <- splits
        , let -- Add the induction hypothesis
              tr_split = tr_hyp ++ split_clauses
              -- We take the dependencies in the contract using f_concl
              deps_split = split_deps `union` delete (Function f_concl) deps_concl
        ]

data Split = Split
    { split_clauses :: [Clause']
    , split_deps    :: [HCCContent]
    , split_num     :: Int
    }

-- Chop this contract up in several parts, enables us to "cursor" through the
-- definition of the function instead of trying it in one go
trSplit :: CoreExpr -> Contract -> TransM [Split]
trSplit expr contract = do
    let f = fromMaybe (error "trSplit: topVar returned Nothing") (topVar expr)

    bind_parts <- getBindParts f

    -- We throw away the parts with min rhs, and look at the min-sets
    -- stored in bind_mins instead
    let decl_parts = filter (not . minRhs . bind_rhs) bind_parts

    -- We will equate the result of the function to the arguments to
    -- the contract
    let contract_args :: [Var]
        contract_args = (map fst . fst . telescope inf) contract

    -- The contract only needs to be translated once
    (tr_contr,ptr_deps) <- first (axioms . splitFormula)
        <$> capturePtrs' (trContract Neg Skolemise expr contract)

    -- The rest of the work is carried out by the bindToSplit function,
    -- by iterating over the (non-min) BindParts.
    zipWithM (bindToSplit f contract_args tr_contr ptr_deps) decl_parts [0..]

bindToSplit :: Var -> [Var] -> [Clause'] -> [HCCContent]
            -> HCCBindPart -> Int -> TransM Split
bindToSplit f contract_args tr_contr contr_deps decl_part@BindPart{..} num = do

    (tr_part,part_ptrs) <- lift $ capturePtrs $ do

        -- Translate just this bind part
        tr_decl <- definitions . splitFormula <$> trBindPart decl_part

        -- Foreach argument e, match up the variable v
        -- introduce e's arguments as skolems and make them equal
        let sks :: [Var]
            sks = concatMap exprFVs bind_args

        -- Everything under here considers the otherwise quantified
        -- vars in the arguments as skolem variables, now quantified
        -- over the whole theory instead
        tr_goal <- local (addSkolems (nub $ sks ++ contract_args)) $ do

            -- Make the arguments to the function equal to the
            -- contract variables from the telescope
            tr_eqs <- axioms .:
                zipWith (===) <$> mapM (trExpr . Var) contract_args
                              <*> mapM trExpr bind_args

            -- Translate the constraints, but instead of having them
            -- as an antecedents, they are now asserted
            tr_constrs <- axioms <$> trConstraints bind_constrs

            -- Translate the relevant mins
            tr_min <- axioms <$> mapM (liftM (foralls . min') . trExpr) bind_mins

            return $ [comment "Imposed min"] ++ tr_min
                  ++ [comment "Equalities from arguments"] ++ tr_eqs
                  ++ [comment "Imposed constraints"] ++ tr_constrs

        return ([comment $ "Bind part for " ++ show f] ++ tr_decl ++ tr_goal)

    -- Use the dependencies defined in the BindPart, but don't add the
    -- dependency to f, or else the full original definition is added
    -- to the theory as well.
    let dep = delete (Function f) $ contr_deps ++ bind_deps ++ part_ptrs

    return $ Split
        { split_clauses = tr_part ++ [comment "Contract"] ++ tr_contr
        , split_deps    = dep
        , split_num     = num
        }
-}

trStmt :: Statement -> TransM [Clause']
trStmt = fmap (clauseSplit axiom) . go Neg True
  where
    go :: Variance -> Bool -> Statement -> TransM Formula'
    go v   top   (e ::: c)
        = trContract v (if top && v == Neg then Skolemise else Quantify) e c
    go Neg True  (Lambda vs u) = local' (addSkolems vs) (go Neg True u)
    go Neg False (Lambda vs u) = exists' vs <$> go Neg True u
    go Pos top   (Lambda vs u) = forall' vs <$> go Pos top u
    go Neg top   (s :=> t)     = (/\) <$> go Pos False s <*> go Neg top t
    go Pos _     (s :=> t)     = (\/) <$> go Neg False s <*> go Pos False t
    go Neg True  (Using s u)   = (/\) <$> go Neg True s <*> go Pos False u
    go v   top   (Using s _)   = go v top s


trContract :: Variance -> Skolem -> CoreExpr -> Contract -> TransM Formula'
trContract variance skolemise_init e_init contract = do

    Params{no_pull_quants,no_skolemisation} <- getParams

    -- Skolemise unless no_skolemisation is on
    let skolemise
            | no_skolemisation = Quantify
            | otherwise        = skolemise_init

    -- We obtain all arguments (to the right of the last arrow), and
    -- bind them under the same quantifier. This makes somewhat
    -- simpler theories (unless no_pull_quants is set)
    let depth              = if no_pull_quants then one else inf
        (arguments,result) = telescope depth contract
        vars               = map fst arguments
        e_result           = foldl (@@) e_init (map Var vars)

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
                    Neg -> min' ex /\ min' px /\ ex =/= unr /\
                                            (px === false \/ px === bad)
                    Pos -> min' ex ==>
                           (min' px /\ (ex === unr \/ px === unr \/ px === true))

            CF -> do
                e_tr <- lift $ trExpr e_result
                return $ case variance of
                    Neg -> min' e_tr /\ neg (cf e_tr)
                    Pos -> min' e_tr ==> cf e_tr

            And c1 c2 -> case variance of { Neg -> ors ; Pos -> ands }
                <$> mapM (trContract variance skolemise e_result) [c1,c2]

            -- This case only happens when no_pull_quants is on
            -- (see telescoping above)
            Arrow{} -> trContract variance skolemise e_result result

        return $ tr_arguments ++ [tr_result]

    return $ case variance of
        Neg -> (skolemise == Quantify ? exists' vars) (ands tr_contract)
        Pos -> forall' vars (ors tr_contract)

local' :: (HaloEnv -> HaloEnv) -> TransM a -> TransM a
local' k m = do
    e <- ask
    lift $ (local k) (runReaderT m e)

capturePtrs' :: TransM a -> TransM (a,[HCCContent])
capturePtrs' m = do
    e <- ask
    lift $ capturePtrs (runReaderT m e)
