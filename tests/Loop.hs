module Loop where

import Prelude (Bool(..))

import Contracts

data Nat = Z | S Nat

isZero Z = True
isZero _ = False

not True  = False
not False = True

loop :: Nat
loop = loop

-- Loop is crash free

unsat_loop_cf = loop ::: CF

-- Loop satisfies everything!

unsat_loop_zero      = loop ::: Pred isZero

unsat_loop_isnt_zero = loop ::: Pred (\x -> not (isZero x))

-- A contract C for a function that is recursive do not
-- have the property
--
-- sat (Theory `union` not C) ==> Theory |- not C
--
-- Does this hold in general?
--
-- Dimitrios thinks that this only breaks when there are recursive
-- functions.
--
-- What about predicates for recursive functions?

loop_pred :: a -> Bool
loop_pred = loop_pred

id x = x

-- This is satisfiable, even though loop_pred *should* be UNR
unsat_id_loop_pred = id ::: CF --> Pred loop_pred

recursive_true :: Nat -> Bool
recursive_true Z     = True
recursive_true (S x) = recursive_true x

-- This is satisfiable,
-- but we need CF --> CF && {x | True} to recursive_true (untested)
sat_id_recursive_true = id ::: CF --> Pred recursive_true

recursive_true_and_cf = recursive_true ::: CF --> CF :&: Pred id
recursive_true_cf = recursive_true ::: CF --> CF

id_four = id (S (S (S (S Z))))

-- A small unit test, four is recursive_true (and crash-free!)
unsat_id_four_rec_true = id_four ::: CF :&: Pred recursive_true


{-
   With dimitrios new style
./Loop.loop_cf_base.tptp, should be SAT
OK from paradox

./Loop.loop_cf_step.tptp, should be SAT
OK from paradox

./Loop.loop_zero_base.tptp, should be SAT
OK from paradox

./Loop.loop_zero_step.tptp, should be SAT
paradox timed out
All tools timed out

./Loop.loop_isnt_zero_base.tptp, should be SAT
OK from paradox

./Loop.loop_isnt_zero_step.tptp, should be SAT
paradox timed out
All tools timed out

./Loop.recursive_true_and_cf_base.tptp, should be SAT
paradox timed out
All tools timed out

./Loop.recursive_true_and_cf_step.tptp, should be SAT
paradox timed out
All tools timed out

./Loop.recursive_true_cf_base.tptp, should be SAT
paradox timed out
All tools timed out

./Loop.recursive_true_cf_step.tptp, should be SAT
paradox timed out
All tools timed out

./Loop.sat_id_loop_pred.tptp, should be SAT
OK from paradox

./Loop.sat_id_recursive_true.tptp, should be SAT
OK from paradox

-}


