module MapFromJust where

import Contracts
import Prelude(Bool(..),error)

data Maybe a = Just a | Nothing

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust Nothing  = False

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = error "fromJust!"

map :: (a -> b) -> [a] -> [b]
map f (x:xs) = f x : map f xs
map f []     = []

all :: (a -> Bool) -> [a] -> Bool
all p (x:xs) = p x && all p xs
all p []     = True

True  && b = b
False && _ = False

map_fromJust_contr = map fromJust ::: CF :&: Pred (all isJust) --> CF
