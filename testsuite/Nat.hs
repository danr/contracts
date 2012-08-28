module Nat where

import Contracts
import Prelude (Bool(..),not,id,($))
import Properties

data Nat = S Nat | Z

(+) :: Nat -> Nat -> Nat
Z   + y = y
S x + y = S (x + y)

(*) :: Nat -> Nat -> Nat
Z   * _ = Z
S x * y = y + (x * y)

(==) :: Nat -> Nat -> Bool
Z     == Z     = True
Z     == _     = False
(S _) == Z     = False
(S x) == (S y) = x == y

(/=) :: Nat -> Nat -> Bool
x /= y = not (x == y)

(<=) :: Nat -> Nat -> Bool
Z     <= _     = True
_     <= Z     = False
(S x) <= (S y) = x <= y

(>) :: Nat -> Nat -> Bool
Z     > _     = False
_     > Z     = True
S x   > S y   = x > y

max :: Nat -> Nat -> Nat
max Z y         = y
max x Z         = x
max (S x) (S y) = S (max x y)

min :: Nat -> Nat -> Nat
min Z y         = Z
min x Z         = Z
min (S x) (S y) = S (min x y)

ind :: Nat -> Nat
ind Z     = Z
ind (S x) = S (ind x)

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

double :: Nat -> Nat
double Z = Z
double (S x) = S (S (double x))

ack :: Nat -> Nat -> Nat
ack Z     n     = S n
ack (S m) Z     = ack m (S Z)
ack (S m) (S n) = ack m (ack (S m) n)

eq_cf         = (==) ::: CF --> CF --> CF

plus_cf       = (+) ::: CF --> CF --> CF

mul_cf        = (*) ::: CF --> CF --> CF
  `Using` plus_cf

broken_mul_cf = (*) ::: CF --> CF --> CF

le_cf         = (<=) ::: CF --> CF --> CF

gt_cf         = (>) ::: CF --> CF --> CF

ne_cf         = (/=) ::: CF --> CF --> CF
  `Using` eq_cf

broken_ne_cf  = (/=) ::: CF --> CF --> CF

max_cf        = max ::: CF --> CF --> CF

min_cf        = min ::: CF --> CF --> CF

double_cf     = double ::: CF --> CF

even_cf       = even ::: CF --> CF

half_cf       = half ::: CF --> CF

ack_cf        = ack ::: CF --> CF --> CF

mult_cf                    = mult ::: CF --> CF --> CF --> CF
  `Using` plus_cf

broken_mult_cf_broken      = mult ::: CF --> CF --> CF --> CF

fac_cf                     = fac ::: CF --> CF
  `Using` mul_cf

broken_fac_cf_broken       = fac ::: CF --> CF

qfac_cf                    = qfac ::: CF --> CF --> CF
  `Using` mul_cf

broken_qfac_cf_broken      = qfac ::: CF --> CF --> CF

exp_cf                     = exp ::: CF --> CF --> CF
  `Using` mul_cf

broken_exp_cf_broken       = exp ::: CF --> CF --> CF

exp_accum_cf               = exp_accum ::: CF --> CF --> CF --> CF
  `Using` mul_cf

broken_exp_accum_cf_broken = exp_accum ::: CF --> CF --> CF --> CF

-- Some more involving properties about equality

eq_refl = reflexive (==)

eq_sym = symmetric (==)

eq_trans = transitive (==)
  `Using` eq_cf
  `Using` eq_refl
  `Using` eq_sym

(<=>) :: Bool -> Bool -> Bool
True  <=> True  = True
False <=> False = True
True  <=> False = False
False <=> True  = False

eq_refl_broken = All (\x -> x ::: CF :=> True ::: Pred ((x == x) <=>))

eq_sym_boolean = All (\x -> All (\y -> x ::: CF :=> y ::: CF :=>
    (y == x ::: CF :&: Pred ((x == y) <=>))))

eq_sym_boolean2_ = (==) ::: CF :-> \x -> CF :-> \y -> CF :&: Pred ((y == x) <=>)

-- Some properties about max and min and plus

max_idem          = max `idempotentOver` (==)
min_idem          = min `idempotentOver` (==)

max_comm          = max `commutativeOver` (==) `Using` eq_refl
max_comm_broken   = max `commutativeOver` (==)

min_comm          = min `commutativeOver` (==)

max_assoc         = max `associativeOver` (==) `Using` eq_refl
max_assoc_broken  = max `associativeOver` (==)

min_assoc         = min `associativeOver` (==) `Using` eq_refl
min_assoc_broken  = min `associativeOver` (==)

plus_assoc        = (+) `associativeOver` (==) `Using` eq_refl
plus_assoc_broken = (+) `associativeOver` (==)

plus_comm         = (+) `commutativeOver` (==) `Using` eq_refl
plus_comm_broken  = (+) `commutativeOver` (==)

-- Some way of expressing that we do not have (<=) and (>) at the same time

binary_le_gt = (<=) ::: CF :-> \x -> CF :-> \y -> CF :&: Pred (\b -> if b then not (x > y) else x > y)

binary_gt_le = (>)  ::: CF :-> \x -> CF :-> \y -> CF :&: Pred (\b -> if b then not (x <= y) else x <= y)
