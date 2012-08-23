module Reflexivity where

import Prelude hiding ((==),(/=),max)
import Contracts

data Nat = Z | S Nat

max :: Nat -> Nat -> Nat
max Z y         = y
max x Z         = x
max (S x) (S y) = S (max x y)

(==) :: Nat -> Nat -> Bool
Z     == Z     = True
(S x) == (S y) = x == y
_     == _     = False

x /= y = not (x == y)

(<=>) :: Bool -> Bool -> Bool
True  <=> True  = True
False <=> False = True
_     <=> _     = False

