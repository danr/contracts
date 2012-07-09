module Shrink where

import Contracts
import Prelude(Bool(..),error)

data Maybe a = Just a | Nothing

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust Nothing  = False

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = error "fromJust!"

unsat_fromJust_contr = fromJust ::: CF :&: Pred isJust --> CF

all :: (a -> Bool) -> [a] -> Bool
all p (x:xs) = p x && all p xs
all p []     = True

True  && b = b
False && _ = False

nonEmpty [] = False
nonEmpty (_:_) = True

shrink :: (a -> a -> a) -> [Maybe a] -> a
shrink (*) []          = error "Empty list!"
shrink (*) (Nothing:_) = error "Nothing!"
shrink (*) [Just x]    = x
shrink (*) (Just x:xs) = x * shrink (*) xs

shrink_lazy :: (a -> a -> a) -> [Maybe a] -> a
shrink_lazy (*) []     = error "Empty list!"
shrink_lazy (*) [x]    = fromJust x
shrink_lazy (*) (x:xs) = fromJust x * shrink_lazy (*) xs

big_unsat_shrink = shrink
    ::: (CF --> CF --> CF) --> CF :&: Pred nonEmpty :&: Pred (all isJust) --> CF

big_sat_shrink_lazy = shrink_lazy
    ::: (CF --> CF --> CF) --> CF :&: Pred nonEmpty :&: Pred (all isJust) --> CF
