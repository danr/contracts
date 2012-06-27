
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
    And x y     -> if invariant x then invariant y else False
    Or x y      -> if invariant x then invariant y else False
    Not x       -> invariant x

    -- Base case
    Var x       -> True

-- | Negating a formula
neg :: Formula v -> Formula v
neg (And f1 f2) = Or  (neg f2) (neg f1)
neg (Or f1 f2)  = And (neg f2) (neg f1)
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
    = neg ::: CF :&: Pred invariant --> CF :&: Pred invariant

{- counter:

and(False,False) = x/True
and(False,BAD) = UNR
and(False,UNR) = UNR
and(False,x/True) = UNR
and(BAD,False) = UNR
and(BAD,BAD) = UNR
and(BAD,UNR) = UNR
and(BAD,x/True) = UNR
and(UNR,False) = UNR
and(UNR,BAD) = UNR
and(UNR,UNR) = UNR
and(UNR,x/True) = UNR
and(x/True,False) = UNR
and(x/True,BAD) = UNR
and(x/True,UNR) = UNR
and(x/True,x/True) = UNR

app(False,False) = False
app(BAD,False) = BAD
app(UNR,False) = UNR
app(x/True,False) = UNR
app(X1,BAD) = app(X1,False)
app(X1,UNR) = app(X1,BAD)
app(X1,x/True) = app(X1,UNR)

bad = BAD

cf(False) <=> $true
cf(BAD) <=> $false
cf(UNR) <=> $true
cf(x/True) <=> $true

false = False

invariant(False) = False
invariant(BAD) = BAD
invariant(UNR) = UNR
invariant(x/True) = UNR

neg_concl(False) = x/True
neg_concl(BAD) = UNR
neg_concl(UNR) = UNR
neg_concl(x/True) = False

neg_hyp(False) = False
neg_hyp(BAD) = False
neg_hyp(UNR) = x/True
neg_hyp(x/True) = UNR

not(False) = UNR
not(BAD) = UNR
not(UNR) = UNR
not(x/True) = UNR

or(False,False) = False
or(False,BAD) = UNR
or(False,UNR) = UNR
or(False,x/True) = UNR
or(BAD,False) = UNR
or(BAD,BAD) = UNR
or(BAD,UNR) = UNR
or(BAD,x/True) = UNR
or(UNR,False) = UNR
or(UNR,BAD) = UNR
or(UNR,UNR) = UNR
or(UNR,x/True) = UNR
or(x/True,False) = UNR
or(x/True,BAD) = UNR
or(x/True,UNR) = UNR
or(x/True,x/True) = UNR

p_0_and(False) = UNR
p_0_and(BAD) = False
p_0_and(UNR) = BAD
p_0_and(x/True) = False

p_0_not(False) = False
p_0_not(BAD) = BAD
p_0_not(UNR) = BAD
p_0_not(x/True) = UNR

p_0_or(False) = False
p_0_or(BAD) = False
p_0_or(UNR) = BAD
p_0_or(x/True) = UNR

p_0_var(False) = False
p_0_var(BAD) = UNR
p_0_var(UNR) = UNR
p_0_var(x/True) = UNR

p_1_and(False) = False
p_1_and(BAD) = UNR
p_1_and(UNR) = x/True
p_1_and(x/True) = False

p_1_or(False) = False
p_1_or(BAD) = False
p_1_or(UNR) = BAD
p_1_or(x/True) = UNR

var(False) = UNR
var(BAD) = UNR
var(UNR) = UNR
var(x/True) = UNR

-}
