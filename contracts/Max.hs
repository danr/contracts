module Max where

import Contracts
import Prelude (Bool(..),otherwise)

data Nat = S Nat | Z

max :: Nat -> Nat -> Nat
max Z y         = y
max x Z         = x
max (S x) (S y) = S (max x y)

max_cf = max ::: CF --> CF --> CF