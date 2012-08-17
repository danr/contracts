module AppendLength where

import Contracts
import Prelude (Bool(..))

data Nat = S Nat | Z

-- Length

length :: [a] -> Nat
length []     = Z
length (_:xs) = S (length xs)

length_cf = length ::: CF --> CF

-- Plus

(+) :: Nat -> Nat -> Nat
Z     + y = y
(S x) + y = S (x + y)

plus_cf = (+) ::: CF --> CF --> CF

-- Equality

id x = x

(==) :: Nat -> Nat -> Bool
Z     == Z     = True
Z     == _     = False
(S _) == Z     = False
(S x) == (S y) = x == y

-- Equality is CF
eq_cf = (==) ::: CF --> CF --> CF

-- Equality is reflexive
eq_refl = All (\x -> x ::: CF :=> x == x ::: CF :&: Pred id)

-- Equality is symmetric
-- TODO: fix this
eq_sym_broken =
    All (\x -> All (\y ->
        (x ::: CF) :=>
        (y ::: CF) :=>
        (x == y ::: CF :&: Pred id) :=>
        (y == x ::: CF :&: Pred id)))
  `Using` eq_cf

-- Append

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- Requires reflexivity of (==)
append_length =
    ((++) ::: CF :-> \xs -> CF :-> \ys ->
              CF :&: Pred (\zs -> length zs == (length xs + length ys)))
  `Using` eq_refl
  `Using` plus_cf
  `Using` length_cf

-- Reverse

{-# CONTRACT reverse1, reverse2 :: {x | True} -> {r | length x == length r} #-}
reverse1 [] = []
reverse1 (y:ys) = reverse1 ys ++ [y]

reverse1_length_broken =
    (reverse1 ::: CF :-> \xs -> CF :&: Pred (\ys -> length xs == length ys))
  `Using` append_length
  `Using` eq_refl
  `Using` length_cf

reverse2 x = rev x []

{-# CONTRACT rev :: {x | True} -> {y | True} -> {r | length r == length x + length y} #-}
rev x y = case x of
    [] -> y
    (a:b) -> rev b (a : y)
