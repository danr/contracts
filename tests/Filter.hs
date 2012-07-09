module Filter where

import Prelude(Bool(..),otherwise)
import Contracts

True  && b = b
False && _ = False

all :: (a -> Bool) -> [a] -> Bool
all p []     = True
all p (x:xs) = p x && all p xs

True  || _ = True
False || b = b

any :: (a -> Bool) -> [a] -> Bool
any p []     = False
any p (x:xs) = p x || any p xs

nonEmpty []    = False
nonEmpty (_:_) = True

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

unsat_any_cf = any ::: (CF --> CF) --> CF --> CF

unsat_filter_all =
    filter ::: (CF --> CF) :-> \p ->
               CF :-> \xs ->
               (CF :&: Pred (all p))

{- Cannot solve these

-- Cannot apply IH when you are at [x], and p x
-- Can we use disjunction here? Is it admissible?
unsat_filter_any_nonempty =
    filter ::: ((CF --> CF) :-> \p ->
                (CF :&: Pred (any p)) :-> \xs ->
                (CF :&: Pred nonEmpty))
  `Using` unsat_any_cf

-- Needs lemmas about <=. Don't sure how to express those
unsat_filter_le_length =
    filter ::: ((CF --> CF) -->
                CF :-> \xs ->
                CF :&: Pred (\rs -> length xs <= length rs))
  `Using` unsat_le
  `Using` unsat_length

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

unsat_le :: Statement
unsat_le = (<=) ::: CF --> CF --> CF

unsat_length :: Statement
unsat_length = length ::: CF --> CF

-}
