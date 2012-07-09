module Infinite where

import Prelude (Bool(..),otherwise)
import Contracts

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

unsat_iterate_cf = iterate ::: (CF --> CF) --> CF --> CF

repeat :: a -> [a]
repeat x = x : repeat x

unsat_repeat_cf = repeat ::: CF --> CF

data Tree a = Branch (Tree a) a (Tree a) | Empty

iterTree :: (a -> a) -> (a -> a) -> a -> Tree a
iterTree f g x = Branch (iterTree f g (f x)) x (iterTree f g (g x))

lefts :: Tree a -> [a]
lefts (Branch _ x xs) = x : lefts xs
lefts Empty           = []

unsat_iterTree_cf = iterTree ::: (CF --> CF) --> (CF --> CF) --> CF --> CF

unsat_lefts_cf = lefts ::: CF --> CF

