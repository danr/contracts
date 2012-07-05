module Contracts.Axioms where

import DataCon
import Id
import Name
import Outputable
import SrcLoc
import TyCon
import Type
import TysWiredIn
import Unique

import Halo.FOL.Abstract
import Halo.Data
import Halo.Subtheory

import Contracts.Theory

-- | Make axioms about CF
mkCF :: [TyCon] -> [HCCSubtheory]
mkCF ty_cons = do
    ty_con <- ty_cons
    let DataTyCon cons _ = algTyConRhs ty_con

    return $ Subtheory
        { provides    = Specific (CrashFree ty_con)
        , depends     = []
        , description = "CF " ++ showSDoc (pprSourceTyCon ty_con)
        , formulae    = concat $
            [
                [ cf kxbar | arity == 0] ++

                [ forall' vars $ [cf kxbar] ===> ands (map cf xbar) | arity > 0] ++

                -- min(K xs) /\ not (cf (K xs)) ==> BigOr_i (min(x_i) /\ not (cf (x_i))
                [ forall' vars $ min' kxbar : [ neg (cf kxbar) ] ===>
                                     ors [ ands [neg (cf x),min' x] | x <- xbar ]
                | arity > 0 ]

            | c <- cons
            , let data_c          = dataConWorkId c
                  (_,_,ty_args,_) = dataConSig c
                  arity           = length ty_args
                  vars            = take arity varNames
                  xbar            = map qvar vars
                  kxbar           = apply data_c xbar
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
