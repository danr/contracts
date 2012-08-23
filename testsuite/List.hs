module List where

import Contracts
import Prelude (Bool(..),not,(&&),(||),otherwise,(.),null)
import Nat

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

length :: [a] -> Nat
length []     = Z
length (_:xs) = S (length xs)

drop :: Nat -> [a] -> [a]
drop Z xs = xs
drop _ [] = []
drop (S x) (_:xs) = drop x xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs


map :: (a -> b) -> [a] -> [b]
map f (x:xs) = f x:map f xs
map f []     = []

zipWith k (x:xs) (y:ys) = k x y : zipWith k xs ys
zipWith k _      _      = []

all :: (a -> Bool) -> [a] -> Bool
all p (x:xs) = p x && all p xs
all p []     = True

any :: (a -> Bool) -> [a] -> Bool
any p []     = False
any p (x:xs) = p x || any p xs

qreverse :: [a] -> [a] -> [a]
qreverse []     acc = acc
qreverse (x:xs) acc = qreverse xs (x:acc)

rotate :: Nat -> [a] -> [a]
rotate Z     xs     = xs
rotate _     []     = []
rotate (S n) (x:xs) = rotate n (xs ++ [x])

elem :: Nat -> [Nat] -> Bool
elem _ [] = False
elem n (x:xs) = n == x || elem n xs

subset :: [Nat] -> [Nat] -> Bool
subset []     ys = True
subset (x:xs) ys = x `elem` xs && subset xs ys

intersect,union :: [Nat] -> [Nat] -> [Nat]
(x:xs) `intersect` ys | x `elem` ys = x:(xs `intersect` ys)
                      | otherwise   = xs `intersect` ys
[]     `intersect` ys = []

union (x:xs) ys | x `elem` ys = union xs ys
                | otherwise   = x:(union xs ys)
union []     ys = ys

isort :: [Nat] -> [Nat]
isort [] = []
isort (x:xs) = insert x (isort xs)

insert :: Nat -> [Nat] -> [Nat]
insert n [] = [n]
insert n (x:xs) =
  case n <= x of
    True -> n : x : xs
    False -> x : (insert n xs)

count :: Nat -> [Nat] -> Nat
count n (x:xs) | n == x = S (count n xs)
               | otherwise = count n xs
count n [] = Z

listEq :: [Nat] -> [Nat] -> Bool
listEq []     []     = True
listEq (x:xs) (y:ys) = x == y && (xs `listEq` ys)
listEq _      _      = False

sorted :: [Nat] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted _        = True

length_cf    = length ::: CF --> CF

append_cf    = (++) ::: CF --> CF --> CF

reverse_cf    = reverse ::: CF --> CF
  `Using` append_cf

filter_all           = filter ::: (CF --> CF) :-> \p -> CF :-> \xs -> CF :&: Pred (all p)

filter_all_only_pred = filter ::: (CF --> CF) :-> \p -> CF :-> \xs -> Pred (all p)

-- Cannot solve
filter_any_nonempty_broken =
    filter ::: ((CF --> CF) :-> \p ->
                (CF :&: Pred (any p)) :-> \xs ->
                (CF :&: Pred (not . null)))
  `Using` any_cf

-- Needs lemmas about <=. Don't sure how to express those
filter_le_length_broken =
    filter ::: ((CF --> CF) -->
                CF :-> \xs ->
                CF :&: Pred (\rs -> length xs <= length rs))

broken_reverse_cf_broken  = reverse ::: CF --> CF

map_cf     = map ::: (CF --> CF) --> CF --> CF

zipWith_cf = zipWith ::: (CF --> CF --> CF) --> CF --> CF --> CF

any_cf = any ::: (CF --> CF) --> CF --> CF

all_cf = all ::: (CF --> CF) --> CF --> CF

all_homomorphism = All (\p -> (++) ::: Pred (all p) --> Pred (all p) --> Pred (all p))

app_any_morphism_bool =
    All (\p -> (p ::: CF --> CF) :=>
            (++) ::: CF :-> \xs -> CF :-> \ys ->
                     CF :&: Pred (\zs -> any p zs <=> (any p xs || any p ys)))
    `Using` any_cf

drop_cf      = drop ::: CF --> CF --> CF

rotate_cf    = rotate ::: CF --> CF --> CF
 `Using`
   append_cf

sorted_cf    = sorted ::: CF --> CF
  `Using` le_cf

elem_cf      = elem ::: CF --> CF --> CF
  `Using` eq_cf

subset_cf    = subset ::: CF --> CF --> CF
  `Using` elem_cf

intersect_cf = intersect ::: CF --> CF --> CF
  `Using` elem_cf

union_cf     = union ::: CF --> CF --> CF
  `Using` elem_cf

count_cf     = count ::: CF --> CF --> CF
  `Using` eq_cf

insert_cf    = insert ::: CF --> CF --> CF
  `Using` le_cf

isort_cf     = isort ::: CF --> CF
  `Using` insert_cf

listEq_cf    = listEq ::: CF --> CF --> CF
  `Using` eq_cf

broken_count_cf     = count ::: CF --> CF --> CF

broken_insert_cf    = insert ::: CF --> CF --> CF

broken_isort_cf     = isort ::: CF --> CF

broken_ne_cf        = (/=) ::: CF --> CF --> CF

broken_listEq_cf    = listEq ::: CF --> CF --> CF

