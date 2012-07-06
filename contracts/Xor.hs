
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

-- | Negating a formula
neg :: Formula v -> Formula v
neg (Xor f1 f2) = Xor (neg f1) (neg f2)
neg Lit         = Lit

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
    = neg ::: (CF --> CF)
          :&: ((CF :&: Pred invariant) :-> \ y -> (CF :&: Pred invariant))
    `Using`
      invariant_cf
    `Using`
      and_cf

{-

   Wow, this is not true:

     y = Xor UNR x
     x = Xor Lit x

     invariant Lit = True
     invariant UNR = UNR
     invariant y = UNR (= invariant UNR && invariant x = UNR && False = UNR)
     invariant x = False (= invariant Lit && invariant x = invariant x)

   but this is non-sensical, invariant x should be UNR.

-}
