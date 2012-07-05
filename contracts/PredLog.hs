
module PredLog where

import Prelude(Bool(..))
import Contracts

data Formula v
    = And (Formula v) (Formula v)
    | Or (Formula v) (Formula v)
    | Not (Formula v)
    | Var v

invariant :: Formula v -> Bool
invariant f = case f of

    -- We should never have two consecutive not
    Not Not{} -> False

    -- Otherwise, recurse
    And x y     -> invariant x && invariant y
    Or x y      -> invariant x && invariant y
    Not x       -> invariant x

    -- Base case
    Var x       -> True

-- | Negating a formula
neg :: Formula v -> Formula v
neg (And f1 f2) = Or  (neg f1) (neg f2)
neg (Or f1 f2)  = And (neg f1) (neg f2)
neg (Not f)     = f
neg (Var x)     = Var x

(&&) :: Bool -> Bool -> Bool
True  && b = b
False && _ = False

and_cf = (&&) ::: CF --> CF --> CF

-- | Invariant is crash free
invariant_cf = invariant ::: CF --> CF

-- | Negating is crash free
neg_contr_cf = neg ::: CF --> CF

-- | Negating retains the invariant /and/ is crash free
neg_contr_retain
    = neg ::: (CF :-> \ x -> CF)
          :&: (CF :&: Pred invariant :-> \ y -> CF :&: Pred invariant)
    `Using`
      invariant_cf
    `Using`
      and_cf
