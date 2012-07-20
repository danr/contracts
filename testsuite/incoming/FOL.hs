module FOL where

import Prelude(Bool(..))
import Contracts

data Term q s
    = Fun s [Term q s]
    | Var q

data Formula q s
    = Equal (Term q s) (Term q s)
    | Unequal (Term q s) (Term q s)
    | And [Formula q s]
    | Or  [Formula q s]
    | Implies (Formula q s) (Formula q s)
    | Neg (Formula q s)
    | Forall [q] (Formula q s)
    | Exists [q] (Formula q s)

    -- what if I derive fmap? hmm... it will depend on the []-functor
    -- which I do not have the code for... but I guess we should be
    -- able to check contracts for some derived functions already


isAnd And{} = True
isAnd _     = False

isOr Or{} = True
isOr _    = False

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

    -- Binary operator list should be at least two elements
    And xs | tinyList xs -> False
    Or xs  | tinyList xs -> False

    -- Binary operators should not directly contain themselves
    And xs | any isAnd xs -> False
    Or xs  | any isOr xs -> False

    -- Otherwise, recurse
    Equal{}     -> True
    Unequal{}   -> True
    And xs      -> all invariant xs
    Or  xs      -> all invariant xs
    Implies x y -> invariant x && invariant y
    Neg x       -> invariant x
    Forall _ xs -> invariant xs
    Exists _ xs -> invariant xs

-- | Negating a formula
neg :: Formula q v -> Formula q v
neg (Neg f)         = f
neg (Equal t1 t2)   = Unequal t1 t2
neg (Unequal t1 t2) = Equal t1 t2
neg (And fs)        = Or (map neg fs)
neg (Or fs)         = And (map neg fs)
neg (Implies f1 f2) = neg f2 `Implies` neg f1
neg (Forall as f)   = Exists as (neg f)
neg (Exists as f)   = Forall as (neg f)

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

