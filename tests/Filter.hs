module Filter where

import Prelude(Bool(..),otherwise)
import Contracts

True  && b = b
False && _ = False

all :: (a -> Bool) -> [a] -> Bool
all p []     = True
all p (x:xs) = p x && all p xs

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

unsat_filter_all_contr =
    filter ::: (CF --> CF) :-> \p ->
               CF :-> \xs ->
               (CF :&: Pred (all p))