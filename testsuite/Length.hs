module Length where

import Contracts
import Prelude (Bool(..),(||),not,error)
import Nat
import List

-- Length

map_length = map ::: (CF --> CF) --> CF :-> \xs -> CF :&: Pred (\ys -> length xs == length ys)

zipWith_length = zipWith
    ::: (CF --> CF --> CF) -->
        CF :-> \xs ->
        CF :-> \ys ->
        CF :&: Pred (\zs -> length zs == min (length xs) (length ys))

-- From ghc/utils/Util.lhs
zipEqual []     []     = []
zipEqual (a:as) (b:bs) = (a,b) : zipEqual as bs
zipEqual _      _      = error "zipEqual: unequal lists"

zipEqual_cf = zipEqual
    ::: CF :-> \xs ->
        CF :&: Pred (\ys -> length ys == length xs) -->
        CF :&: Pred (\zs -> length zs == length xs)


--     (==) :: Length.Nat -> Length.Nat -> GHC.Types.Bool
--     (S ?1) == (S ?1) =  False
-- But somehow asserting that (+) is CF works around this lack of reflexivity
drop_length_broken = drop
    ::: CF :-> \n ->
        CF :-> \xs ->
        CF :&: Pred (\ys -> n + length ys == length xs)


-- Says:
--     n :: Length.Nat
--     n =  S n
--
--     xs :: [a]
--     xs =  []
drop_length_2_broken = drop
    ::: (CF :-> \n ->
        CF :-> \xs ->
        CF :&: Pred (\ys -> n + length ys == length xs))
  `Using`
    ((+) ::: CF --> CF --> CF)
-- a true counterexample, since "fix S + length (drop (fix S) []) != length []"



-- Now:
--     n :: Length.Nat
--     n =  Z
--
--     xs :: [a]
--     xs =  ?4 : xs
drop_length_3_broken = drop
    ::: (CF :-> \n ->
        CF :-> \xs ->
        CF :&: Pred (\ys -> min n (length xs) + length ys == length xs))
  `Using`
    ((+) ::: CF --> CF --> CF)


-- We're stuck at reflexivity now, but it seems to require a lot of other things too.
--
--    (==) :: Length.Nat -> Length.Nat -> GHC.Types.Bool
--    (S ?1) == (S ?1) =  False
drop_length_4_broken = drop
    ::: (CF :-> \n ->
        CF :-> \xs ->
        CF :&: Pred (\ys -> (n + length ys == length xs) || (n > length xs)))
   `Using` ((+) ::: CF --> CF --> CF)
   `Using` gt_cf

-- dropWhile

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p []     = []
dropWhile p (x:xs) = if p x then dropWhile p xs else x:xs


{-

    The counterexample here is really dull:

        p ?1           = True
        xs             = ?1 : xs
        dropWhile p xs = xs

-}
dropWhile_oops = dropWhile
    ::: ((CF --> CF) :-> \p -> CF :-> \xs -> CF :&: Pred (\ys -> not (any p ys)))
    `Using` any_cf


-- Same counterexample
dropWhile_oops_non_rec = dropWhile
    ::: ((CF --> CF) :-> \p -> CF :-> \xs -> CF :&: Pred (\ys -> not (any p ys)))
    `Using` (dropWhile ::: (CF --> CF) --> CF --> CF)
    `Using` any_cf


-- This one holds
dropWhile_suffix = dropWhile
    ::: (CF --> CF) :-> \p -> CF :-> \xs -> CF :&: Pred (suffixNot p)


suffixNot p []     = True
suffixNot p (x:xs) = not (p x)

