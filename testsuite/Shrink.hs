module Shrink where

import Contracts
import Prelude(Bool(..),Maybe(..),error,(&&),not,(.),null)
import Data.Maybe (fromJust)
import List

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust Nothing  = False

fromJust_contr = fromJust ::: CF :&: Pred isJust --> CF

shrink :: (a -> a -> a) -> [Maybe a] -> a
shrink op []          = error "Empty list!"
shrink op (Nothing:_) = error "Nothing!"
shrink op [Just x]    = x
shrink op (Just x:xs) = x `op` shrink op xs

shrink_lazy :: (a -> a -> a) -> [Maybe a] -> a
shrink_lazy op []     = error "Empty list!"
shrink_lazy op [x]    = fromJust x
shrink_lazy op (x:xs) = fromJust x `op` shrink_lazy op xs

-- Less efficient to use (not . null) :(
-- nonEmpty = not . null
nonEmpty (_:_) = True
nonEmpty []    = False

shrink_cf = shrink
    ::: (CF --> CF --> CF) --> CF :&: Pred nonEmpty :&: Pred (all isJust) --> CF

broken_shrink_lazy = shrink_lazy
    ::: (CF --> CF --> CF) :->
        \ op -> CF :&: Pred nonEmpty :&: Pred (all isJust) :->
        \ xs -> CF

simpler :: (a -> b) -> Maybe a -> b
simpler op (Just x) = op x
simpler op Nothing  = error "Nothing!"

simpler_cf = simpler ::: (CF --> CF) --> CF :&: Pred isJust --> CF

simpler_lazy :: (a -> b) -> Maybe a -> b
simpler_lazy op m = op (fromJust m)

simpler_lazy_cf = simpler_lazy ::: (CF --> CF) --> CF :&: Pred isJust --> CF

combine op m1 m2 = fromJust m1 `op` fromJust m2

combine_cf = combine ::: (CF --> CF --> CF) --> CF :&: Pred isJust --> CF :&: Pred isJust --> CF
