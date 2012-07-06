
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

and_cf = (&&) ::: CF --> CF --> CF

-- | Invariant is crash free
invariant_cf = invariant ::: CF --> CF

ind_contr_cf = ind ::: CF --> CF

-- | Indating retains the invariant /and/ is crash free
ind_contr_retain
    = ind -- ::: (CF --> CF)
          ::: (Pred invariant :-> 
                       \ y -> (Pred (\r -> invariant y && invariant r)))
          -- :&: (CF :&: Pred invariant --> Pred invariant)
    `Using`
      invariant_cf
    `Using`
      and_cf
      
               

