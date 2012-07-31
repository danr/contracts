module Contracts.Axioms where

import Outputable
import TyCon
import Type

import Halo.FOL.Abstract
import Halo.PrimCon
import Halo.Names
import Halo.Shared
import Halo.Subtheory

import Contracts.Theory

import Control.Monad

-- | Make axioms about CF
mkCF :: [TyCon] -> [HCCSubtheory]
mkCF ty_cons = do
    ty_con <- ty_cons
    guard (isAlgTyCon ty_con)
    DataTyCon dcs _ <- [algTyConRhs ty_con]

    return $ Subtheory
        { provides    = Specific (CrashFree ty_con)
        , depends     = []
        , description = "CF " ++ showSDoc (pprSourceTyCon ty_con)
        , formulae    = concat $
            [
                [ cf kxbar | arity == 0] ++

                [ foralls $ [cf kxbar] ===> ands (map cf xbar) | arity > 0 ] ++

                -- min(K xs) /\ not (cf (K xs)) ==> BigOr_i (min(x_i) /\ not (cf (x_i))
                [ foralls $ min' kxbar : [ neg (cf kxbar) ] ===>
                                 ors [ ands [neg (cf y),min' y] | y <- xbar ]
                | arity > 0 ]

            | dc <- dcs
            , let (k,arity)       = dcIdArity dc
                  args            = take arity varNames
                  xbar            = map qvar args
                  kxbar           = apply k xbar
            ]
        }

-- | The essentials about BAD and UNR
primConAxioms :: HCCSubtheory
primConAxioms = Subtheory
    { provides    = Specific PrimConAxioms
    , depends     = []
    , description = "Axioms for BAD and UNR"
    , formulae    =
         [ cf unr
         , neg (cf bad)
         , unr =/= bad
         , forall' [x] $ [ x' =/= unr, cf x'] ===> min' x'
         ]
    }

-- | App on BAD and UNR
primConApps :: HCCSubtheory
primConApps = Subtheory
    { provides    = Specific PrimConApps
    , depends     = [ AppOnMin ]
    , description = "App on BAD and UNR"
    , formulae    =
         [ forall' [x] $ app bad x' === bad
         , forall' [x] $ app unr x' === unr
         ]
    }
