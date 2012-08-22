module TryNat where

import Contracts
import Nat
import Prelude ()

ind_cf  = ind ::: CF --> CF

plus_cf = (+) ::: CF --> CF --> CF

mul_cf  = (*) ::: CF --> CF --> CF
  `Using` plus_cf

max_cf  = max ::: CF --> CF --> CF

min_cf  = min ::: CF --> CF --> CF

eq_cf   = (==) ::: CF --> CF --> CF

ne_cf   = (/=) ::: CF --> CF --> CF
  `Using` eq_cf

le_cf   = (<=) ::: CF --> CF --> CF


