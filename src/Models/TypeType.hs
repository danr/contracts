{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-

    The Typelike instance for GHC's Type.

-}
module Models.TypeType where

import Type
import Unify
import Outputable
import TysPrim

import Models.Show (Typelike(..))
import Models.Model

import Halo.Shared

import Data.Maybe

instance Typelike Type where
    eqTy   = eqType

    showTy = showSDoc . ppr

    peel (Arity n) t = splitFunTysN n (repType t)

    split = splitFunTys . repType

    -- add clause r = AnyType -> True ?
    lg r s = r `eqType` anyTy || isJust (getSubst r s)

    -- add clause r = AnyType -> identity ? How to do it recursively?
    unifySubst r s
        | r `eqType` anyTy = id
        | otherwise = case getSubst r s of
            Nothing -> error $ "cannot unify these!"
                                ++ showTy r ++ " and " ++ showTy s
            Just si -> \t -> substTy si (repType t)

-- Using repType to get rid of extra foralls and go through type and
-- newtype synonyms

getSubst :: Type -> Type -> Maybe TvSubst
getSubst (repType -> r) (repType -> s) = sigma
  where
    vs    = tyVarsOfType r
    sigma = tcMatchTy vs r s
    _tr   = trace $ unlines
        ["r: " ++ showOutputable r
        ,"s: " ++ showOutputable s
        ,"vs: " ++ showOutputable vs
        ,"sigma: " ++ showOutputable sigma
        ]


