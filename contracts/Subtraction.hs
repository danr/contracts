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

le_cf = (<=) ::: CF --> CF --> CF

minus_contr =
    (-) ::: CF :-> \x -> CF :&: Pred (\y -> y <= x) --> CF

minus_contr_patched =
    (-) ::: CF :-> \x -> CF :&: Pred (\y -> y <= x) --> CF
  `Using` le_cf

