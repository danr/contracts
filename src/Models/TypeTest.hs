{-# LANGUAGE ParallelListComp #-}
module Main where

import GHC
import GHC.Paths
import Id
import Name
import OccName
import Outputable
import SrcLoc
import Type
import TysWiredIn
import Unify
import Unique
import VarSet
import MonadUtils

import Control.Monad

vars@(a':b':rest') =
    [ mkLocalId
        (mkInternalName (mkUnique 'z' i) (mkOccName varName n) wiredInSrcSpan)
        liftedTypeKind
    | i <- [0..]
    | n <- ([1..] >>= flip replicateM "abcxyz")
    ]

a:b:rest = map mkTyVarTy vars

tys =
    [ mkFunTy boolTy boolTy
    , mkForAllTy a' $ mkFunTy a boolTy
    , mkForAllTy a' $ mkFunTy (mkListTy a) boolTy
    , mkFunTy a boolTy
    , mkFunTy (mkListTy a) boolTy
    , mkForAllTy b' $ mkFunTy b boolTy
    , mkForAllTy b' $ mkFunTy (mkListTy b) boolTy
    , mkFunTy b boolTy
    , mkFunTy (mkListTy b) boolTy
    ]

{-
unifs = [ (vs,a,b,tcMatchTy emptyVarSet a b)
        | a <- tys
        , b <- tys
        , let vs = tyVarsOfType a
        ]

showUnif (vs,a,b,match) = showSDoc $ hang
    (text "tcMatchTy") 4 $
    vcat
        [ppr vs
        ,parens (ppr a)
        ,parens (ppr b) <+> equals
        ,ppr match]
        -}

unifs = [ (a,b,tcUnifyTys (const BindMe) [a] [b])
        | a <- tys
        , b <- tys
        ]

showUnif (a,b,match) = showSDoc $ hang
    (text "tcUnifyTys") 4 $
    vcat
        [parens (ppr a)
        ,parens (ppr b) <+> equals
        ,ppr match]

main :: IO ()
main = runGhc (Just libdir) $ liftIO $ mapM_ (putStrLn . (++ "\n") . showUnif) unifs
