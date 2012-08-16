module Mc91 where

import Contracts
import Prelude (Bool(..),error)

data Nat = S Nat | Z

(==) :: Nat -> Nat -> Bool
Z     == Z     = True
Z     == _     = False
(S _) == Z     = False
(S x) == (S y) = x == y

(+) :: Nat -> Nat -> Nat
Z     + y = y
(S x) + y = S (x + y)

False || x = x
_     || _ = True

True  && x = x
_     && _ = False

(-) :: Nat -> Nat -> Nat
x   - Z   = x
Z   - _   = error "Negative Nat!"
S x - S y = x - y

(<=) :: Nat -> Nat -> Bool
Z   <= _   = True
_   <= Z   = False
S x <= S y = x <= y

(>) :: Nat -> Nat -> Bool
x > y = S y <= x

-- CF of (<=) is not needed because they have the same recursive structure
minus_cf   = (-) ::: CF :-> \x -> CF :&: Pred (\y -> y <= x) --> CF

plus_cf    = (+) ::: CF --> CF --> CF

le_cf      = (<=) ::: CF --> CF --> CF

eq_cf      = (==) ::: CF --> CF --> CF

iter10 f x = f (f (f (f (f (f (f (f (f (f x)))))))))

n100       = iter10 (iter10 S) Z

n10        = iter10 S Z

n11        = S n10

n91        = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

{-# CONTRACT f91 :: {n | True}
    -> {r | ((n <= 100 && r == 91) || n > 100 && r == n - 10)} #-}

{-
f91 :: Int -> Int
f91 n = case (n <= 100) of
    True -> f91 (f91 (n + 11))
    False -> n - 10
    -}


f91 :: Nat -> Nat
f91 n = case n <= n100 of
    True  -> f91 (f91 (n + n11))
    False -> n - n100

f91_contr = (f91 ::: CF :-> \n ->
    CF :&: Pred (\r -> ((n <= n100) && (r == n91)) || ((n > n100) && (r == (n - n10)))))
  `Using` minus_cf
  `Using` plus_cf
  `Using` le_cf
  `Using` eq_cf
