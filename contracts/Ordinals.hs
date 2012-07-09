{- These require

 all f . { min(f)& [ all x. (min(f @ x) & cf(x)) => cf(f @ x) ] } => cf(f)

 all f . { min(f)& [ exists x. (min(f @ x) & cf(x)) => ~cf(f @ x) ] } => ~cf(f)

   which we don't have

-}

module Ordinals where

import Contracts
import Prelude ()

data Nat = Z | S Nat

data Ord = Zero | Suc Ord | Lim (Nat -> Ord)

(++) :: Ord -> Ord -> Ord
Zero  ++ y = y
Suc x ++ y = Suc (x ++ y)
Lim f ++ y = Lim (\n -> f n ++ y)

(**) :: Ord -> Ord -> Ord
Zero  ** y = Zero
Suc x ** y = y ++ (x ** y)
Lim f ** y = Lim (\n -> f n ** y)

unsat_ord_plus_cf = (++) ::: CF --> CF
unsat_ord_mul_cf  = (**) ::: CF --> CF `Using` unsat_ord_plus_cf
