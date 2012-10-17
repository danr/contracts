module Recursion where

import Contracts
import Prelude (Bool(..),otherwise)

data Nat = S Nat | Z

True  && x = x
_ && _ = False

False || x = x
_  || _ = True

length :: [a] -> Nat
length []     = Z
length (_:xs) = S (length xs)

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

drop :: Nat -> [a] -> [a]
drop Z xs = xs
drop _ [] = []
drop (S x) (_:xs) = drop x xs

ind :: Nat -> Nat
ind Z     = Z
ind (S x) = S (ind x)

unsat_ind_cf = ind ::: CF --> CF

max :: Nat -> Nat -> Nat
max Z y         = y
max x Z         = x
max (S x) (S y) = S (max x y)

-- Bug
-- max_cf = max ::: CF --> CF --> CF

min :: Nat -> Nat -> Nat
min Z y         = Z
min x Z         = Z
min (S x) (S y) = S (min x y)

-- Bug
-- min_cf = min ::: CF --> CF --> CF

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

qrev :: [a] -> [a] -> [a]
qrev []     acc = acc
qrev (x:xs) acc = qrev xs (x:acc)

double :: Nat -> Nat
double Z = Z
double (S x) = S (S (double x))

even :: Nat -> Bool
even Z = True
even (S Z) = False
even (S (S x)) = even x

half :: Nat -> Nat
half Z = Z
half (S Z) = Z
half (S (S x)) = S (half x)

mult :: Nat -> Nat -> Nat -> Nat
mult Z     _ acc = acc
mult (S x) y acc = mult x y (y + acc)

fac :: Nat -> Nat
fac Z = S Z
fac (S x) = S x * fac x

qfac :: Nat -> Nat -> Nat
qfac Z     acc = acc
qfac (S x) acc = qfac x (S x * acc)

exp :: Nat -> Nat -> Nat
exp _ Z     = S Z
exp x (S n) = x * exp x n

exp_accum :: Nat -> Nat -> Nat -> Nat
exp_accum x Z     acc = acc
exp_accum x (S n) acc = exp_accum x n (x * acc)

(+),(*) :: Nat -> Nat -> Nat
Z     + y = y
(S x) + y = S (x + y)

Z     * _ = Z
(S x) * y = y + (x * y)

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

(==),(/=) :: Nat -> Nat -> Bool
Z     == Z     = True
Z     == _     = False
(S _) == Z     = False
(S x) == (S y) = x == y

x /= y = not (x == y)

not True  = False
not False = True

listEq :: [Nat] -> [Nat] -> Bool
listEq []     []     = True
listEq (x:xs) (y:ys) = x == y && (xs `listEq` ys)
listEq _      _      = False

Z     <= _     = True
_     <= Z     = False
(S x) <= (S y) = x <= y

sorted :: [Nat] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted _        = True

unsat_and_cf       = (&&) ::: CF --> CF --> CF
unsat_or_cf        = (||) ::: CF --> CF --> CF

unsat_length_cf    = length ::: CF --> CF

unsat_append_cf    = (++) ::: CF --> CF --> CF

unsat_rev_cf       = rev ::: CF --> CF
  `Using` unsat_append_cf

sat_rev_cf_broken  = rev ::: CF --> CF

unsat_double_cf    = double ::: CF --> CF

unsat_even_cf      = even ::: CF --> CF

unsat_half_cf      = half ::: CF --> CF

unsat_plus_cf      = (+) ::: CF --> CF --> CF

unsat_mul_cf       = (*) ::: CF --> CF --> CF
  `Using` unsat_plus_cf

sat_mul_cf_broken  = (*) ::: CF --> CF --> CF

unsat_mult_cf      = mult ::: CF --> CF --> CF --> CF
  `Using` unsat_plus_cf

sat_mult_cf_broken = mult ::: CF --> CF --> CF --> CF

unsat_fac_cf       = fac ::: CF --> CF
  `Using` unsat_mul_cf

sat_fac_cf_broken  = fac ::: CF --> CF

unsat_qfac_cf      = qfac ::: CF --> CF --> CF
  `Using` unsat_mul_cf

sat_qfac_cf_broken = qfac ::: CF --> CF --> CF

unsat_exp_cf       = exp ::: CF --> CF --> CF
  `Using` unsat_mul_cf

sat_exp_cf_broken  = exp ::: CF --> CF --> CF

big_unsat_exp_accum_cf      = exp_accum ::: CF --> CF --> CF --> CF
  `Using` unsat_mul_cf

big_sat_exp_accum_cf_broken = exp_accum ::: CF --> CF --> CF --> CF

unsat_not_cf       = not ::: CF --> CF

{- Bug in translation of multi-patterns (!)

   Need to fix this

   These all probably work with optimisation, but
   the translation of optimised contracts is buggy

unsat_drop_cf      = drop ::: CF --> CF --> CF
unsat_eq_cf        = (==) ::: CF --> CF --> CF
unsat_ne_cf        = (/=) ::: CF --> CF --> CF
unsat_rotate_cf    = rotate ::: CF --> CF --> CF
 `Using`
   unsat_append_cf

unsat_count_cf     = count ::: CF --> CF --> CF
unsat_listEq_cf    = listEq ::: CF --> CF --> CF
unsat_sorted_cf    = sorted ::: CF --> CF
  `Using` unsat_le_cf

unsat_isort_cf     = isort ::: CF --> CF
  `Using` unsat_insert_cf

unsat_insert_cf    = insert ::: CF --> CF --> CF
  `Using` unsat_eq_cf

unsat_elem_cf      = elem ::: CF --> CF --> CF
  `Using` unsat_eq_cf

unsat_subset_cf    = subset ::: CF --> CF --> CF
  `Using` unsat_elem_cf

unsat_intersect_cf = intersect ::: CF --> CF --> CF
  `Using` unsat_elem_cf

unsat_union_cf     = union ::: CF --> CF --> CF
  `Using` unsat_elem_cf
nd
unsat_le_cf        = (<=) ::: CF --> CF --> CF

-}

