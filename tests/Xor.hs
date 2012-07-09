
module Xor where

import Prelude(Bool(..))
import Contracts

data Formula v
    = Xor (Formula v) (Formula v) | Lit

-- | Everything satisfies the invariant! Hooray!
invariant :: Formula v -> Bool
invariant f = case f of

    Xor x y -> invariant x && invariant y
    Lit     -> True

ind :: Formula v -> Formula v
ind (Xor f1 f2) = Xor (ind f1) (ind f2)
ind Lit         = Lit

(&&) :: Bool -> Bool -> Bool
True  && b = b
False && _ = False

unsat_and_cf = (&&) ::: CF --> CF --> CF

-- | Invariant is crash free
unsat_invariant_cf = invariant ::: CF --> CF

unsat_ind_contr_cf = ind ::: CF --> CF

-- | Ind retains the invariant /and/ is crash free
unsat_ind_contr_retain
    = ind ::: (Pred invariant :->
                \ y -> (Pred (\r -> invariant y && invariant r)))
    `Using`
      unsat_invariant_cf
    `Using`
      unsat_and_cf



         -- ::: (CF --> CF)
         -- :&: (CF :&: Pred invariant --> Pred invariant)
