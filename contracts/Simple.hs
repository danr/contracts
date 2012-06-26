module Simple(ind_cf,eq_cf,ind_same_length) where

import Prelude (Bool(..))
import Contracts

ind :: [Bool] -> [Bool]
ind (x:xs) = x:ind xs
ind []     = []

ind_cf = ind ::: CF --> CF

data Nat = S Nat | Z

length :: [a] -> Nat
length (x:xs) = S (length xs)
length []     = Z

eq Z Z         = True
eq (S n) (S m) = eq n m
eq _ _         = False

eq_cf = eq ::: CF --> CF --> CF

ind_same_length = ind ::: CF :-> \xs -> CF :&: Pred (\ys -> length xs `eq` length ys)
