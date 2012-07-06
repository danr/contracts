module AndLists where

import Prelude(Bool(..))
import Contracts

data Formula
    = And [Formula]
    | Or  [Formula]
    | Implies (Formula) (Formula)
    | Neg (Formula)
    | Lit Bool

isAnd And{} = True
isAnd _     = False

isOr Or{} = True
isOr _    = False

invariant :: Formula -> Bool
invariant f = case f of

    -- We should never have two consecutive negations
    Neg Neg{} -> False

    -- Binary operator list should be at least two elements,
    -- and operators should not directly contain themselves
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

all_cf = all ::: (CF --> CF) --> CF --> CF

-- It would be cool if we could prove properties like this:
map_pred p
    = map ::: ((Pred p --> Pred p) --> (Pred (all p) --> Pred (all p)))
  `Using`
     (p ::: CF --> CF)
  `Using`
     invariant_cf
  `Using`
     all_cf

map_invariant = map
    ::: (Pred invariant :-> \x -> Pred (\ r -> invariant x && invariant r))
    --> (Pred (all invariant) :-> \xs -> Pred (\rs -> all invariant xs && all invariant rs))

invariant_cf = invariant ::: CF --> CF

-- | Negating retains the invariant
neg_contr = neg ::: (Pred invariant
                :-> \x -> Pred (\r -> invariant x && invariant r))
  `Using`
    invariant_cf
  `Using`
    map_invariant

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
