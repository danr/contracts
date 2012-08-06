module Rotate where

import Contracts
import Prelude (Bool(..),otherwise)

data Nat = S Nat | Z

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

rotate :: Nat -> [a] -> [a]
rotate Z     xs     = xs
rotate _     []     = []
rotate (S n) (x:xs) = rotate n (xs ++ [x])

append_cf    = (++) ::: CF --> CF --> CF

rotate_cf    = rotate ::: CF --> CF --> CF
  `Using`
    append_cf
