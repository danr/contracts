module Nat where

import Prelude(Bool(..))

data Nat = S Nat | Z

ind :: Nat -> Nat
ind Z     = Z
ind (S x) = S (ind x)

max :: Nat -> Nat -> Nat
max Z y         = y
max x Z         = x
max (S x) (S y) = S (max x y)


min :: Nat -> Nat -> Nat
min Z y         = Z
min x Z         = Z
min (S x) (S y) = S (min x y)

(+) :: Nat -> Nat -> Nat
Z   + y = y
S x + y = S (x + y)

(*) :: Nat -> Nat -> Nat
Z   * _ = Z
S x * y = y + (x * y)

(==) :: Nat -> Nat -> Bool
Z     == Z     = True
Z     == _     = False
(S _) == Z     = False
(S x) == (S y) = x == y

(/=) :: Nat -> Nat -> Bool
x /= y = if x == y then False else True

(<=) :: Nat -> Nat -> Bool
Z     <= _     = True
_     <= Z     = False
(S x) <= (S y) = x <= y


