module Eq where

import Contracts
import Prelude(Bool(..))

not True  = False
not False = True

data Nat = Zero | Succ Nat

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

    x == y = not (x /= y)
    x /= y = not (x == y)

instance Eq Bool where
    True  == x = x
    False == x = not x

eq_bool_cf  = ((==) :: Bool -> Bool -> Bool) ::: CF --> CF --> CF

neq_bool_cf = ((/=) :: Bool -> Bool -> Bool) ::: CF --> CF --> CF

instance Eq Nat where
    Zero   == Zero   = True
    Succ x == Succ y = x == y
    _      == _      = False

-- Cannot inline these
eq_nat_cf   = ((==) :: Nat -> Nat -> Bool) ::: CF --> CF --> CF

neq_nat_cf  = ((/=) :: Nat -> Nat -> Bool) ::: CF --> CF --> CF
