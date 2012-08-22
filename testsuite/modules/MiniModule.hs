module MiniModule where

import Prelude ()

data Nat = S Nat | Z

(+) :: Nat -> Nat -> Nat
Z   + y = y
S x + y = S (x + y)

