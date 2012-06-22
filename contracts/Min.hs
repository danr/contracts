module Min where

import Prelude hiding (length)

import Contracts

-- Important with min is that the discrimination axioms are guarded
-- which gives the possibility to collapse everything else to a single
-- point if it is minimises the true occurences of min.
data Nat = Z | S Nat

length [] = Z
length (x:xs) = S (length xs)

isZero Z = True
isZero _ = False

-- This contract is not true, so we would expect that the generated theory
-- is satisfiable.
--
-- Without min, eprover diverges on this example,
-- but with min translation even eprover gives us a model (satisfiable).
-- Paradox should give us a really small (probably four-element) model
-- when using min.
weird_contr = length ::: CF --> Pred isZero