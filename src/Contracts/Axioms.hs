{-# LANGUAGE RecordWildCards,ViewPatterns #-}
{-

    The built-in axioms about CF, UNR and BAD

-}
module Contracts.Axioms where

import Outputable
import TyCon
import Type
import TysPrim
import Var

import Halo.BackgroundTheory
import Halo.Conf
import Halo.FOL.Abstract
import Halo.PrimCon
import Halo.Names
import Halo.Shared
import Halo.Subtheory

import Contracts.HCCTheory
import Contracts.Params

import Control.Monad

import Data.Maybe
import Data.List

impliesOr :: Formula q v -> [Formula q v] -> Formula q v
phi `impliesOr` [] = neg phi
phi `impliesOr` xs = phi ==> ors xs


-- | Make axioms about CF
mkCF :: [TyCon] -> [HCCSubtheory]
mkCF ty_cons = do
    ty_con <- ty_cons
    guard (not (isNewTyCon ty_con))
    let dcs = tyConDataCons ty_con

    return $ Subtheory
        { provides    = Specific (CrashFree ty_con)
        , depends     = []
        , description = "CF " ++ showSDoc (pprSourceTyCon ty_con)
        , formulae    = concat $
            [
                -- cf(K xs) ==> BigAnd_i (cf (x_i))
                [ foralls $ cf kxbar ==> ands (map cf xbar')
                | not (null xbar') ] ++
                [ foralls $ ands (map cf xbar') ==> cf kxbar
                | not (null xbar') ] ++
                [ cf kxbar | null xbar' ]
{- New CF:
                -- min(K xs) /\ not (cf (K xs))
                --    ==> BigOr_i (min(x_i) /\ not (cf (x_i))
                [ foralls $ (min' kxbar /\ neg (cf kxbar)) `impliesOr`
                                 [ ands [neg (cf y),min' y] | y <- xbar' ]
                ]
-}
            | dc <- dcs
            , let (k,arg_types) = dcIdArgTypes dc
                  args          = zipWith setVarType varNames arg_types
                  xbar          = map qvar args
                  is_primitive  = (`eqType` intPrimTy) . varType
                  xbar'         = map qvar (filter (not . is_primitive) args)
                  kxbar         = apply k xbar
            ]
        }

-- | The essentials about BAD and UNR
primConAxioms :: Params -> HCCSubtheory
primConAxioms Params{..} = Subtheory
    { provides    = Specific PrimConAxioms
    , depends     = []
    , description = "Axioms for BAD and UNR"
    , formulae    =
         [ cf unr
         , neg (cf bad)
         , unr =/= bad
-- New CF:         , forall' [x] $ [ x' =/= unr, cf x'] ===> min' x'
         ] 
--        ++ [forall' [x]  $ minrec x' ==> min' x'] ++ 
--         [ forall' [x] $ min' x' \/ x' === unr | min_or_unr ]
    }

-- | App on BAD and UNR
primConApps :: HCCSubtheory
primConApps = Subtheory
    { provides    = Specific PrimConApps
    , depends     = []
    , description = "App on BAD and UNR"
    , formulae    =
         [ forall' [x] $ app bad x' === bad
         , forall' [x] $ app unr x' === unr
         ]
    }

-- | Make constructors of different types and pointers disjoint
--
--   We use this to easier print well-typed models from paradox.
extraDisjoint :: HaloConf -> [Subtheory s] -> [Clause']
extraDisjoint halo_conf subthys = map (clause axiom) $
    -- Make all data constructors disjoint from pointers
    [ makeDisjoint halo_conf d p
    | ds <- tycons, d <- ds, p <- ptrs ] ++
    -- Make all data constructors of different types disjoint
    [ makeDisjoint halo_conf d1 d2
    | (d1s,d2ss) <- zip tycons (drop 1 (tails tycons))
    , d1 <- d1s , d2s <- d2ss , d2 <- d2s ]
  where
    isData (provides -> Data ty_con) = Just (ty_con_disj ty_con)
    isData _                         = Nothing

    isPtr (provides -> Pointer p)    = Just (pointer_disj p)
    isPtr _                          = Nothing

    tycons = mapMaybe isData subthys

    ptrs   = mapMaybe isPtr subthys

    ty_con_disj :: TyCon -> [Disjoint]
    ty_con_disj ty_con =
        [ Disjoint {..}
        | let dcs = tyConDataCons ty_con
        , dc <- dcs
        , let (symbol,arg_types) = dcIdArgTypes dc
              min_guard          = False
              is_ptr             = False
        ]

    pointer_disj :: Var -> Disjoint
    pointer_disj p = Disjoint
        { symbol    = p
        , arg_types = []
        , min_guard = False
        , is_ptr    = True
        }
