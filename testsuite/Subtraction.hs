module Subtraction where

import Contracts
import Prelude(Bool(..),otherwise,error)

data Nat = Succ Nat | Zero

(-) :: Nat -> Nat -> Nat
x      - Zero   = x
Zero   - _      = error "Negative Nat!"
Succ x - Succ y = x - y

(<=) :: Nat -> Nat -> Bool
Zero   <= _      = True
_      <= Zero   = False
Succ x <= Succ y = x <= y

-- CF of (<=) is not needed because they have the same recursive structure
minus_cf = (-) ::: CF :-> \x -> CF :&: Pred (\y -> y <= x) --> CF

minus_cf_broken = (-) ::: CF --> CF --> CF

minus_cf_broken_2 = (-) ::: CF :-> \x -> CF :&: Pred (\y -> y <= Succ x) --> CF
-- using < instead of <=



