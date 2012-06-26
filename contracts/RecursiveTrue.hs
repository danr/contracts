module RecursiveTrue where

import Prelude (Bool(..))

import Contracts

data Nat = Z | S Nat

id x = x

recursive_true :: Nat -> Bool
recursive_true Z     = True
recursive_true (S x) = recursive_true x

-- Seems to be a bug here
recursive_true_and_cf = recursive_true ::: CF --> CF :&: Pred id
