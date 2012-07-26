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

-- | We want to access the params and the fix info while doing this
type TransM = ReaderT (Params,FixInfo) HaloM

getParams :: TransM Params
getParams = asks fst

getFixInfo :: TransM FixInfo
getFixInfo = asks snd

data Skolem = Skolemise | Quantify
  deriving (Eq,Ord,Show)

trTopStmt :: TopStmt -> TransM [Conjecture]
trTopStmt (TopStmt _name stmt deps) =
    trStatement deps (stripTreeUsings stmt)

trStatement :: [HCCContent] -> Statement -> TransM [Conjecture]
trStatement deps stmt@Statement{..} = do

    Params{..} <- getParams

    fpi_content <- trFPI deps stmt
    plain_content <- trPlain deps trNeg Skolemise stmt

    let conjectures
            | fpi_no_plain && not (null fpi_content) = fpi_content
            | otherwise = plain_content : fpi_content

    (using_clauses,using_deps) <- (`mapAndUnzipM` statement_using) $ \used_stm ->
        local' (addSkolems statement_args) $ do
            Conjecture u_cl u_dep _ <- trPlain deps (\_sk -> trPos) Quantify used_stm
            return $ (u_cl,u_dep)

    let extender = extendConj (concat using_clauses) (concat using_deps)

    return $ map extender conjectures

trPlain :: [HCCContent] -> (Skolem -> CoreExpr -> Contract -> TransM Formula')
        -> Skolem -> Statement -> TransM Conjecture
trPlain deps tr_fun sk stmt@(Statement e c as _) = do

    (tr_contr,ptrs) <- capturePtrs' $ ($ tr_fun sk e c) $
        case sk of
            Skolemise -> local' (addSkolems as)
            Quantify  -> fmap (forall' as)

    let clauses =
            [comment (show stmt)
            ,(`clause` tr_contr) $ case sk of
                        Skolemise -> negatedConjecture
                        Quantify  -> hypothesis
            ]

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
                return . clause negatedConjecture <$>
                trNeg Skolemise (e_subst f_base) (rename_c ConstantUNR)

            (tr_step,ptrs_step) <- capturePtrs' $ do
                hyp   <- clause hypothesis <$>
                            trPos (e_subst f_hyp) (rename_c Hyp)
                concl <- clause negatedConjecture <$>
                            trNeg Skolemise (e_subst f_concl) (rename_c Concl)
                return [hyp,concl]

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

trPos :: CoreExpr -> Contract -> TransM Formula'
trPos e c = do
    lift $ write $ "trPos"
                ++ "\n  e:" ++ showExpr e
                ++ "\n  c:" ++ show c
    case c of
        Pred p -> do
            ex <- lift $ trExpr e
            px <- lift $ trExpr p
            return $ min' ex ==> (min' px /\ (ex === unr \/ px === unr \/ px === true))
        CF -> do
            e_tr <- lift $ trExpr e
            return $ min' e_tr ==> cf e_tr
        And c1 c2 -> (/\) <$> trPos e c1 <*> trPos e c2
        Arrow v c1 c2 -> do
            l <- trNeg Quantify (Var v) c1
            r <- trPos (e @@ Var v) c2
            return $ forall' [v] (l \/ r)

trNeg :: Skolem -> CoreExpr -> Contract -> TransM Formula'
trNeg skolemise e c = do
    lift $ write $ "trNeg (" ++ show skolemise ++ ")"
                ++ "\n  e:" ++ showExpr e
                ++ "\n  c:" ++ show c
    case c of
        Pred p -> do
            ex <- lift $ trExpr e
            px <- lift $ trExpr p
            return $ min' ex /\ min' px /\ ex =/= unr /\ (px === false \/ px === bad)

        CF -> do
            e_tr <- lift $ trExpr e
            return $ min' e_tr /\ neg (cf e_tr)

        And c1 c2 -> (\/) <$> trNeg skolemise e c1
                          <*> trNeg skolemise e c2

        Arrow v c1 c2 -> local' (skolemise == Skolemise ? addSkolem v) $ do
            l <- trPos (Var v) c1
            r <- trNeg skolemise (e @@ Var v) c2
            return $ (skolemise == Quantify ? exists' [v]) (l /\ r)

local' :: (HaloEnv -> HaloEnv) -> TransM a -> TransM a
local' k m = do
    e <- ask
    lift $ (local k) (runReaderT m e)

capturePtrs' :: TransM a -> TransM (a,[Var])
capturePtrs' m = do
    e <- ask
    lift $ capturePtrs (runReaderT m e)
