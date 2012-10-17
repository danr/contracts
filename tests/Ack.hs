module Ack where

import Prelude (Bool(..),otherwise)
import Contracts

data Nat = S Nat | Z

(+) :: Nat -> Nat -> Nat
Z   + y = y
S x + y = S (x + y)

unsat_plus_cf      = (+) ::: CF --> CF --> CF

ack :: Nat -> Nat -> Nat
ack Z     n     = S n
ack (S m) Z     = ack m (S Z)
ack (S m) (S n) = ack m (ack (S m) n)

big_unsat_ack_cf       = ack ::: CF --> CF --> CF
  `Using`
    unsat_plus_cf
