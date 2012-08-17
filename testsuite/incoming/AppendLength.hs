module AppendLength where

import Contracts
import Prelude (Bool(..))

data Nat = S Nat | Z

length :: [a] -> Nat
length []     = Z
length (_:xs) = S (length xs)

(+) :: Nat -> Nat -> Nat
Z     + y = y
(S x) + y = S (x + y)


-- Append

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- Needs reflexivity of ==:
-- fof(x, axiom, ! [X,L] : ((L = 'f_=='(X,X) & min(L)) => (L = c_UNR | L = c_True))).
append_length_broken = (++)
    ::: CF :-> \xs -> CF :-> \ys ->
        CF :&: Pred (\zs -> length zs == (length xs + length ys))

-- Reverse

{-# CONTRACT reverse1, reverse2 :: {x | True} -> {r | length x == length r} #-}
reverse1 x = case x of
    [] -> []
    (y:ys) -> reverse1 ys ++ [y]

reverse2 x = rev x []

{-# CONTRACT rev :: {x | True} -> {y | True} -> {r | length r == length x + length y} #-}
rev x y = case x of
    [] -> y
    (a:b) -> rev b (a : y)
