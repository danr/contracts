module SimpleEq where

import Contracts
import Prelude(Bool(..))

not True  = False
not False = True

data Nat = Zero | Succ Nat

class Eq a where
    (==) :: a -> a -> Bool

instance Eq Bool where
    True  == x = x
    False == x = not x

eq_bool_cf  = ((==) :: Bool -> Bool -> Bool) ::: CF --> CF --> CF

instance Eq Nat where
    Zero   == Zero   = True
    Succ x == Succ y = x == y
    _      == _      = False

eq_nat_cf   = ((==) :: Nat -> Nat -> Bool) ::: CF --> CF --> CF

