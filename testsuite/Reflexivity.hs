module Reflexivity where

import Prelude (Bool(..))
import Contracts

data Nat = Z | S Nat

max :: Nat -> Nat -> Nat
max Z y         = y
max x Z         = x
max (S x) (S y) = S (max x y)

id x = x

(==) :: Nat -> Nat -> Bool
Z     == Z     = True
(S x) == (S y) = x == y
_     == _     = False

x /= y = if x == y then True else False

(<=>) :: Bool -> Bool -> Bool
True  <=> True  = True
False <=> False = True
_     <=> _     = False

(&&) :: Bool -> Bool -> Bool
True  && b = b
False && _ = False

False || b = b
True  || _ = True

f $ x = f x
infixl 0 $

eq_cf = (==) ::: CF --> CF --> CF

eq_refl = All (\x -> x ::: CF :=> x == x ::: CF :&: Pred id)

eq_refl_broken = All (\x -> x ::: CF :=> True ::: Pred ((x == x) <=>))

eq_sym = All (\x -> All (\y -> x ::: CF :=> y ::: CF :=>
    (y == x ::: CF :&: Pred ((x == y) <=>))))

{-
-- This one is Unsatisfiable with min, but Satisfiable without min!
-- Bleh :(
eq_sym' = All $ \x -> All $ \y ->
    x ::: CF :=> y ::: CF :=>
    x == y ::: CF :&: Pred id :=>
    y == x ::: CF :&: Pred id
-}

eq_trans_broken = (All $ \x -> All $ \y -> All $ \z ->
    x ::: CF :=> y ::: CF :=> z ::: CF :=>
    x == y ::: CF :&: Pred id :=>
    y == z ::: CF :&: Pred id :=>
    x == z ::: CF :&: Pred id)
  `Using` eq_cf
  `Using` eq_refl
  `Using` eq_sym

max_refl = All (\x -> x ::: CF :=> max x x ::: CF :&: Pred (== x))
