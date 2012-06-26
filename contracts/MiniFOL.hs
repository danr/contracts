
module MiniFOL where

import Prelude(Bool(..))
import Contracts

data Term q s
    = Fun s [Term q s]
    | Var q

data Formula q s
    = Equal (Term q s) (Term q s)
    | Unequal (Term q s) (Term q s)
    | And (Formula q s) (Formula q s)
    | Or (Formula q s) (Formula q s)
    | Neg (Formula q s)
    | Forall [q] (Formula q s)
    | Exists [q] (Formula q s)

invariant :: Formula q s -> Bool
invariant f = case f of

    -- We should never have two consecutive negations
    Neg Neg{} -> False

    -- We should never have negated equality or inequality
    Neg Equal{} -> False
    Neg Unequal{} -> False

    -- Quantifier lists should never be empty
    Forall [] _ -> False
    Exists [] _ -> False

    -- Otherwise, recurse
    Equal{}     -> True
    Unequal{}   -> True
    And x y     -> invariant x && invariant y
    Or x y      -> invariant x && invariant y
    Neg x       -> invariant x
    Forall _ xs -> invariant xs
    Exists _ xs -> invariant xs

-- | Negating a formula
neg :: Formula q v -> Formula q v
neg (Equal t1 t2)   = Unequal t1 t2
neg (Unequal t1 t2) = Equal t1 t2
neg (And f1 f2)     = Or (neg f2) (neg f1)
neg (Or f1 f2)      = And (neg f2) (neg f1)
neg (Forall as f)   = Exists as (neg f)
neg (Exists as f)   = Forall as (neg f)
neg (Neg f)         = f

-- | Negating retains the invariant
neg_contr = neg ::: CF :&: Pred invariant --> CF :&: Pred invariant

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

tinyList :: [a] -> Bool
tinyList []  = True
tinyList [_] = True
tinyList _   = False

True  && b = b
False && _ = False

False || b = b
True  || _ = True

not True  = False
not False = True
