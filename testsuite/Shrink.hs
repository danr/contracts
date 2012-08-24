module Shrink where

import Contracts
import Prelude(Bool(..),Maybe(..),error,(&&),not,(.),null)
import Data.Maybe (fromJust)
import List
import Nat

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust Nothing  = False

fromJust_contr = fromJust ::: CF :&: Pred isJust --> CF

shrink :: (a -> a -> a) -> [Maybe a] -> a
shrink op []          = error "Empty list!"
shrink op (Nothing:_) = error "Nothing!"
shrink op [Just x]    = x
shrink op (Just x:xs) = x `op` shrink op xs

-- Less efficient to use (not . null) :(
-- nonEmpty = not . null
nonEmpty (_:_) = True
nonEmpty []    = False

shrink_cf = shrink
    ::: (CF --> CF --> CF) --> CF :&: Pred nonEmpty :&: Pred (all isJust) --> CF

-- This is where the magic begins, the contracts for `shrink_lazy' are SAT
-- without min x \/ x = unr, and UNS with it.
-- It takes a while for equinox and paradox to find that they are SAT

shrink_lazy :: (a -> a -> a) -> [Maybe a] -> a
shrink_lazy op []     = error "Empty list!"
shrink_lazy op [x]    = fromJust x
shrink_lazy op (x:xs) = fromJust x `op` shrink_lazy op xs

broken_shrink_lazy = shrink_lazy
    ::: (CF --> CF --> CF) :->
        \ op -> CF :&: Pred nonEmpty :&: Pred (all isJust) :->
        \ xs -> CF

broken_shrink_lazy_using_all_cf = broken_shrink_lazy `Using` all_cf

broken_shrink_lazy_and = shrink_lazy
    ::: (CF --> CF --> CF) :->
        \ op -> CF :&: Pred (\xs -> nonEmpty xs && all isJust xs) :->
        \ xs -> CF

broken_shrink_lazy_and_using_all_cf = broken_shrink_lazy_and `Using` all_cf

-- This exhibits the same behaviour, and could be regarded as simpler
shrink_lazy_def :: (a -> b -> b) -> [Maybe a] -> b -> b
shrink_lazy_def op []     def = def
shrink_lazy_def op (x:xs) def = fromJust x `op` shrink_lazy_def op xs def

broken_shrink_lazy_def = shrink_lazy_def
    ::: (CF --> CF --> CF) :-> \ op ->
        CF :&: Pred (all isJust) :-> \ xs
        -> CF :-> \ def -> CF

broken_shrink_lazy_def_using_all_cf = broken_shrink_lazy_def `Using` all_cf

-- This small HOF and recursive function is SAT without min or unr,
-- bit diverges with it (which is strange, because this contract is in
-- many senses smaller than the one for shrink_lazy)
justMap :: (a -> b) -> [Maybe a] -> [b]
justMap f (x:xs) = f (fromJust x) : justMap f xs
justMap f []     = []

justMap_cf_broken = justMap ::: (CF --> CF) --> CF :&: Pred (all isJust) --> CF

justMap_cf_broken_using_all_cf = justMap_cf_broken `Using` all_cf

-- This is UNSAT regardless, so it is something with the higher-orderness
sum :: [Maybe Nat] -> Nat
sum []     = Z
sum (x:xs) = fromJust x + sum xs

sum_cf = sum ::: CF :&: Pred (all isJust) --> CF
  `Using` plus_cf


-- Failed attempts to do shrink_lazy simpler

simpler :: (a -> b) -> Maybe a -> b
simpler op (Just x) = op x
simpler op Nothing  = error "Nothing!"

simpler_cf = simpler ::: (CF --> CF) --> CF :&: Pred isJust --> CF

simpler_lazy :: (a -> b) -> Maybe a -> b
simpler_lazy op m = op (fromJust m)

simpler_lazy_cf = simpler_lazy ::: (CF --> CF) --> CF :&: Pred isJust --> CF

combine :: (a -> b -> c) -> Maybe a -> Maybe b -> c
combine op m1 m2 = fromJust m1 `op` fromJust m2

combine_cf = combine ::: (CF --> CF --> CF) --> CF :&: Pred isJust --> CF :&: Pred isJust --> CF

