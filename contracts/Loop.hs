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

loop_cf = loop ::: CF

-- Both of these are satisfaible(!)
loop_zero      = loop ::: Pred isZero
loop_isnt_zero = loop ::: Pred (\x -> not (isZero x))

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
