
-- A caricature of a predicate logic negation function satisfying
-- some invariant :)

module PredLogUsing where

import Prelude(Bool(..))
import Contracts

data Formula v
    = Or (Formula v) (Formula v)
    | Not (Formula v)
    | Var v

invariant :: Formula v -> Bool
invariant f = case f of

    Or x y      -> True
    Not x       -> invariant x

    -- Base case
    Var x       -> True

-- | Negating a formula
neg :: Formula v -> Formula v
neg (Or f1 f2)  = Or (neg f1) (neg f2)
neg (Not f)     = f
neg (Var x)     = Var x

-- | Invariant is crashfree
invariant_cf = invariant ::: CF --> CF

-- | Negating is crash free
neg_contr_cf = neg ::: CF --> CF

-- | Negating retains the invariant
neg_contr_retain_invariant = neg ::: Pred invariant --> Pred invariant

-- | Negating retains the invariant AND is crash free
neg_contr_retain_invariant_and_cf
    = neg ::: (CF --> CF)
          :&: (CF :&: Pred invariant --> CF :&: Pred invariant)
      `Using`
         invariant_cf
