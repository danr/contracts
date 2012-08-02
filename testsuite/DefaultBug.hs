module DefaultBug where

import Contracts
import Prelude(Bool(..),error)

data T = A | B | C | D

f :: T -> Bool
f A = True
f B = True
f x = g x

{-# NOINLINE g #-}
g :: T -> Bool
g C = True
g D = True
g x = error "blaaa"
    -- ^ this case never happens, but how defaults are translated
    --   we get untyped things to go to BAD rather than UNR

f_cf = f ::: CF --> CF
