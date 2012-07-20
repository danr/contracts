module Foldr1 where

import Contracts
import Prelude(Bool(..),error)

null [] = True
null xs = False

not True = False
not False = True

f . g = \x -> f (g x)

foldr1           :: (a -> a -> a) -> [a] -> a
foldr1 f [x]     =  x
foldr1 f (x:xs)  =  f x (foldr1 f xs)
foldr1 _ []      =  error "Prelude.foldr1: empty list"

foldr1_cf = foldr1
    ::: (CF --> CF --> CF) --> CF :&: Pred (not . null) --> CF

broken_foldr1_cf_only_pred = foldr1
    ::: (CF --> CF --> CF) --> Pred (not . null) --> CF

broken_foldr1_cf_broken = foldr1
    ::: (CF --> CF --> CF) --> CF --> CF
