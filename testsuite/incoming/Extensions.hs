module Extensions where

import Prelude (Bool(..))
import Contracts

(==>) :: Statement b -> Statement a -> Statement (a,b)
x ==> y = Using y x

infixr 0 ==>

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

eq_refl_broken x = (x ::: CF) ==> x == x ::: CF :&: Pred id

max_refl_broken x = (x ::: CF) ==> max x x ::: CF :&: Pred (== x)
