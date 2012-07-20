module Ack where

import Prelude (Bool(..))
import Contracts

data Nat = S Nat | Z

ack :: Nat -> Nat -> Nat
ack Z     n     = S n
ack (S m) Z     = ack m (S Z)
ack (S m) (S n) = ack m (ack (S m) n)

ack_cf = ack ::: CF --> CF --> CF

