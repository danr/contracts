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

drop_length_broken = drop
    ::: CF :-> \n ->
        CF :-> \xs ->
        CF :&: Pred (\ys -> n + length ys == length xs)

-- Says:
--     (==) :: Length.Nat -> Length.Nat -> GHC.Types.Bool
--     (S ?1) == (S ?1) =  False

drop_length_oops = drop
    ::: (CF :-> \n ->
        CF :-> \xs ->
        CF :&: Pred (\ys -> n + length ys == length xs))
  `Using`
    ((+) ::: CF --> CF --> CF)

-- Says:
-- n :: Length.Nat
-- n =  S n
--
-- xs :: [a]
-- xs =  []


-- True, since "fix S + length (drop (fix S)) != Z"

drop_length_oops_2 = drop
    ::: (CF :-> \n ->
        CF :-> \xs ->
        CF :&: Pred (\ys -> min n (length xs) + length ys == length xs))
  `Using`
    ((+) ::: CF --> CF --> CF)
