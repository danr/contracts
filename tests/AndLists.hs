module AndLists where

import Prelude(Bool(..))
import Contracts

data Formula
    = And [Formula]
    | Or  [Formula]
    | Implies (Formula) (Formula)
    | Neg (Formula)
    | Lit Bool

invariant :: Formula -> Bool
invariant f = case f of

    -- We should never have two consecutive negations
    Neg Neg{} -> False

    -- Binary operator list should be at least two elements, and their
    -- elements should satisfy the invariants
    And xs -> properList xs && all invariant xs
    Or xs  -> properList xs && all invariant xs

    -- None of the above? Recurse
    Implies x y -> invariant x && invariant y
    Neg x       -> invariant x

    -- Base case
    Lit x -> True

-- | Negating a formula
neg :: Formula -> Formula
neg (Neg f)         = f
neg (And fs)        = Or (map neg fs)
neg (Or fs)         = And (map neg fs)
neg (Implies f1 f2) = neg f2 `Implies` neg f1
neg (Lit b)         = Lit b

-- | What it means to retain a predicate
retain :: (a -> Bool) -> Contract (a -> a)
retain p = Pred p :-> \x -> Pred (\ r -> p x && p r)

-- | Invariant is crash free
unsat_all_cf = all ::: (CF --> CF) --> CF --> CF

-- | Invariant is crash free
unsat_invariant_cf = invariant ::: CF --> CF
  `Using` unsat_all_cf

-- | Retaining is preserved by mapping
unsat_map_invariant = map ::: retain invariant --> retain (all invariant)
  `Using` unsat_all_cf

-- | Negating retains the invariant
unsat_neg_contr = neg ::: retain invariant
  `Using`
    unsat_map_invariant
  `Using`
    unsat_invariant_cf
  `Using`
    unsat_all_cf

-- * Auxiliary functions


map :: (a -> b) -> [a] -> [b]
map f (x:xs) = f x:map f xs
map f []     = []

all :: (a -> Bool) -> [a] -> Bool
all p (x:xs) = p x && all p xs
all p []     = True

any :: (a -> Bool) -> [a] -> Bool
any p (x:xs) = p x || any p xs
any p []     = False

properList :: [a] -> Bool
properList []  = False
properList [_] = False
properList _   = True

True  && b = b
False && _ = False

False || b = b
True  || _ = True

not True  = False
not False = True

f . g = \x -> f (g x)

isAnd And{} = True
isAnd _     = False

isOr Or{} = True
isOr _    = False

