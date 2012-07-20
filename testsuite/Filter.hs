module Filter where

import Prelude(Bool(..),otherwise,error)
import Contracts

True  && b = b
False && _ = False

all :: (a -> Bool) -> [a] -> Bool
all p []     = True
all p (x:xs) = p x && all p xs

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

any_pred :: a -> Bool
any_pred _ = error "any_pred!"

all_cf = all ::: (CF --> CF) --> CF --> CF

filter_all =
    filter ::: (CF --> CF) :-> \p ->
               CF :-> \xs ->
               CF :&: Pred (all p)

filter_all_only_pred =
    filter ::: (CF --> CF) :-> \p ->
               CF :-> \xs ->
               Pred (all p)

{- Cannot solve these

-- Cannot apply IH when you are at [x], and p x
-- Can we use disjunction here? Is it admissible?
filter_any_nonempty =
    filter ::: ((CF --> CF) :-> \p ->
                (CF :&: Pred (any p)) :-> \xs ->
                (CF :&: Pred nonEmpty))
  `Using` any_cf

-- Needs lemmas about <=. Don't sure how to express those
filter_le_length =
    filter ::: ((CF --> CF) -->
                CF :-> \xs ->
                CF :&: Pred (\rs -> length xs <= length rs))
  `Using` le
  `Using` length

-- Auxiliary
data Nat = S Nat | Z

length :: [a] -> Nat
length (x:xs) = S (length xs)
length []     = Z

(<=) :: Nat -> Nat -> Bool
Z   <= Z   = True
Z   <= S _ = True
S _ <= Z   = False
S x <= S y = x <= y

le_cf :: Statement
le_cf = (<=) ::: CF --> CF --> CF

length_cf :: Statement
length_cf = length ::: CF --> CF

True  || _ = True
False || b = b

any :: (a -> Bool) -> [a] -> Bool
any p []     = False
any p (x:xs) = p x || any p xs

nonEmpty []    = False
nonEmpty (_:_) = True


-}
