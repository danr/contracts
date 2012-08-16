module Length where

import Contracts
import Prelude (Bool(..))

length :: [a] -> Nat
length []     = Z
length (_:xs) = S (length xs)

min :: Nat -> Nat -> Nat
min Z y         = Z
min x Z         = Z
min (S x) (S y) = S (min x y)

data Nat = S Nat | Z

(+) :: Nat -> Nat -> Nat
Z     + y = y
(S x) + y = S (x + y)

(==) :: Nat -> Nat -> Bool
Z     == Z     = True
Z     == _     = False
(S _) == Z     = False
(S x) == (S y) = x == y

-- Length

zipWith k (x:xs) (y:ys) = k x y : zipWith k xs ys
zipWith k _      _      = []

map k (x:xs) = k x : map k xs
map k []     = []

map_length = map ::: (CF --> CF) --> CF :-> \xs -> CF :&: Pred (\ys -> length xs == length ys)

zipWith_length = zipWith
    ::: (CF --> CF --> CF) -->
        CF :-> \xs ->
        CF :-> \ys ->
        CF :&: Pred (\zs -> length zs == min (length xs) (length ys))

drop :: Nat -> [a] -> [a]
drop Z xs = xs
drop _ [] = []
drop (S x) (_:xs) = drop x xs

-- Says:
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
drop_length_oops = drop
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
drop_length_oops_2 = drop
    ::: (CF :-> \n ->
        CF :-> \xs ->
        CF :&: Pred (\ys -> min n (length xs) + length ys == length xs))
  `Using`
    ((+) ::: CF --> CF --> CF)


-- We're stuck at reflexivity now, but it seems to require a lot of other things too.
--
--    (==) :: Length.Nat -> Length.Nat -> GHC.Types.Bool
--    (S ?1) == (S ?1) =  False
drop_length_maybe_broken = drop
    ::: (CF :-> \n ->
        CF :-> \xs ->
        CF :&: Pred (\ys -> (n + length ys == length xs) || (n > length xs)))
   `Using` ((+) ::: CF --> CF --> CF)
   `Using` gt_cf

Z     > _     = False
_     > Z     = True
S x   > S y   = x > y

gt_cf = (>) ::: CF --> CF --> CF

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

any :: (a -> Bool) -> [a] -> Bool
any p (x:xs) = p x || any p xs
any p []     = False

any_cf = any ::: (CF --> CF) --> CF --> CF

(||) :: Bool -> Bool -> Bool
False || b = b
True  || _ = True

not :: Bool -> Bool
not True  = False
not False = True
