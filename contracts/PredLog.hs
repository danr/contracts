
module PredLog where

import Prelude(Bool(..))
import Contracts

data Formula v
    = And (Formula v) (Formula v)
    | Or (Formula v) (Formula v)
    | Neg (Formula v)
    | Var v

invariant :: Formula v -> Bool
invariant f = case f of

    -- We should never have two consecutive negations
    Neg Neg{} -> False

    -- Otherwise, recurse
    And x y     -> invariant x && invariant y
    Or x y      -> invariant x && invariant y
    Neg x       -> invariant x

    -- Base case
    Var x       -> True

-- | Negating a formula
neg :: Formula v -> Formula v
neg (And f1 f2)     = Or (neg f2) (neg f1)
neg (Or f1 f2)      = And (neg f2) (neg f1)
neg (Neg f)         = f
neg (Var x)         = Var x

-- | Invariant is crashfree
invariant_cf = invariant ::: CF --> CF

-- | Negating is crash free
neg_contr_cf = neg ::: CF --> CF

-- | Negating retains the invariant
neg_contr_retain_invariant = neg ::: Pred invariant --> Pred invariant

-- | Negating retains the invariant AND is crash free
neg_contr_retain_invariant_and_cf = neg ::: CF :&: Pred invariant --> CF :&: Pred invariant

True  && b = b
False && _ = False

and_contr = (&&) ::: CF --> CF --> CF
