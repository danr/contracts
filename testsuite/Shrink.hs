module Shrink where

import Contracts
import Prelude(Bool(..),Maybe(..),error,undefined,(&&),(||),not,(.),null,flip,uncurry,fst,snd,head,tail)
import Data.Maybe (fromJust)
import List
import Nat

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust Nothing  = False

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
--
-- Cannot prove this with induction, we get this counterexample:
--
--    xs = [UNR,Nothing] (obviously CF)
--    justMap_hyp f [Nothing] #= BAD
--
-- The thing is that
--
--    isJust UNR && all isJust [Nothing] = UNR,
--
-- so we cannot appeal to the induction hypothesis
justMap :: (a -> b) -> [Maybe a] -> [b]
justMap f (x:xs) = f (fromJust x) : justMap f xs
justMap f []     = []

justMap_cf_broken = justMap ::: (CF --> CF) --> CF :&: Pred (all isJust) --> CF

justMap_cf_broken_using_all_cf = justMap_cf_broken `Using` all_cf

-- With pattern matching
justMap_pm :: (a -> b) -> [Maybe a] -> [b]
justMap_pm f (Just x:xs) = f x : justMap_pm f xs
justMap_pm f []          = []

justMap_pm_cf = justMap_pm ::: (CF --> CF) --> CF :&: Pred (all isJust) --> CF

justMap_pm_cf_using_all_cf = justMap_pm_cf `Using` all_cf

-- This is UNSAT, but that is because + is strict in its first argument
sum :: [Maybe Nat] -> Nat
sum []     = Z
sum (x:xs) = fromJust x + sum xs

sum_cf = sum ::: CF :&: Pred (all isJust) --> CF
  `Using` plus_cf

-- This has the counterexample [UNR,Nothing], and then the induction
-- hypothesis cannot be appealed to.
-- Doesn't use a HOF
fromJusts :: [Maybe a] -> [a]
fromJusts []     = []
fromJusts (x:xs) = fromJust x : fromJusts xs

fromJusts_cf_broken = fromJusts ::: CF :&: Pred (all isJust) --> CF

-- Trying to prove it with allSat, which is all from both directions ;)
all' :: (a -> Bool) -> [a] -> Bool
all' p (x:xs) = all p xs && p x
all' p []     = True

allSat p = Pred (all p) :&: Pred (all' p)

fromJusts_cf_allSat_broken = fromJusts ::: CF :&: allSat isJust --> CF
fromJusts_cf_allSat_broken_ = fromJusts ::: CF :&: allSat isJust --> CF
    `Using` (all' isJust ::: CF --> CF)
    `Using` (all isJust ::: CF --> CF)

-- An attempt to prove it with some lookahead

fromJusts_cf_head_tail_broken =
    fromJusts
        ::: CF :&: Pred (\xs -> null xs || isJust (head xs))
               :&: Pred (\xs -> null xs || all isJust (tail xs))
        --> CF

-- But writing it in terms of pattern matching is an option
fromJusts_pm :: [Maybe a] -> [a]
fromJusts_pm []          = []
fromJusts_pm (Just x:xs) = x : fromJusts_pm xs

fromJusts_pm_cf = fromJusts_pm ::: CF :&: Pred (all isJust) --> CF

-- We can actually do the same thing without using recursion
unJustPair    (x,y) = (fromJust x,fromJust y)
unJustPair_pm (Just x,Just y) = (x,y)

bothJust (x,y) = isJust x && isJust y

unJustPair_cf_broken = unJustPair ::: CF :&: Pred bothJust --> CF
unJustPair_pm_cf     = unJustPair_pm ::: CF :&: Pred bothJust --> CF

swap = uncurry (flip (,))

-- However, if we make sure to examine that they both are Just, then we're OK
unJustPair_cf     = unJustPair ::: CF :&: Pred bothJust :&: Pred (bothJust . swap) --> CF
unJustPair_cf_alt = unJustPair ::: CF :&: Pred (isJust . fst) :&: Pred (isJust . snd) --> CF


sumMaybes :: (Maybe Nat,Maybe Nat) -> Nat
sumMaybes (x,y) = fromJust x + fromJust y

sumMaybes_cf = sumMaybes ::: CF :&: Pred bothJust --> CF
  `Using` plus_cf

sumMaybes_swap :: (Maybe Nat,Maybe Nat) -> Nat
sumMaybes_swap (x,y) = fromJust y + fromJust x

sumMaybes_swap_cf = sumMaybes ::: CF :&: Pred bothJust --> CF
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
