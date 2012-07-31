module Record where

import Contracts
import Prelude(Bool(..))

not True  = False
not False = True

data Nat = Zero | Succ Nat

data Eq a = Eq
    { (==) :: a -> a -> Bool
    , (/=) :: a -> a -> Bool
    }

mkEq eq = Eq eq (\x y -> not (eq x y))

eq_bool :: Eq Bool
eq_bool = mkEq eq
  where
    True  `eq` x = x
    False `eq` x = not x

eq_nat :: Eq Nat
eq_nat = mkEq eq
  where
    Zero   `eq` Zero   = True
    Succ x `eq` Succ y = x `eq` y
    _      `eq` _      = False

eq_bool_cf        = (==) eq_bool ::: CF --> CF --> CF
neq_bool_cf       = (/=) eq_bool ::: CF --> CF --> CF

eq_nat_cf         = (==) eq_nat ::: CF --> CF --> CF
neq_nat_cf_broken = (/=) eq_nat ::: CF --> CF --> CF
neq_nat_cf        = (/=) eq_nat ::: CF --> CF --> CF
  `Using` eq_nat_cf

