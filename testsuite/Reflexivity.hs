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
Z     == _     = False
(S _) == Z     = False
(S x) == (S y) = x == y

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

eq_cf = (==) ::: CF --> CF --> CF

eq_refl = All (\x -> x ::: CF :=> x == x ::: CF :&: Pred id)

eq_sym = All (\x -> All (\y -> x ::: CF :=> y ::: CF :=>
    (y == x ::: CF :&: Pred ((x == y) <=>))))

eq_trans_broken = All (\x -> All (\y -> All (\z ->
    x ::: CF :=> y ::: CF :=> z ::: CF :=>
    (x == z ::: CF :&: Pred
        (\b ->
            if b
                then (x == y) <=> (y == z)
                else (x /= y) || (y /= z))))))
  `Using` eq_refl

max_refl = All (\x -> x ::: CF :=> max x x ::: CF :&: Pred (== x))
