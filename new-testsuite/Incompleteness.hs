module Incompleteness where

import Contracts
import Prelude (Bool(..),not,id,(&&), (||))

data Nat = S Nat | Z

eq :: Nat -> Nat -> Bool
eq Z Z         = True
eq Z _         = False
eq (S x) Z     = False
eq (S x) (S y) = eq x y

eqB True False = False
eqB False True = False
eqB _ _ = True

recN Z     = True
recN (S x) = recN x

recB True  = True
recB False = True 

isSuc (S x) = True
isSuc Z     = False

flop Z = True
flop (S x) = flop x
flop_ok = flop ::: Pred recN --> Pred (\x -> x)

{- This is a contrived kind of incompleteness induced by 'min' but
 - it can happen. Basically we are using a contract for an expression
 - that we don't directly call but if were to reduce it a few steps down
 - the way, this expression could result to an expression for which we
 - are interested and hence the full power of the contract should be unleashed.

 - This indicates that *definitions* should be translated without /min/ guards
 - to expose as many equalities as possible. On the other hand that seems to
 - threaten finite models (does it? counterexample?)
 -}

{-# NOINLINE glop #-}
glop Z     = flop (S Z)
glop (S x) = flop (S (S x))

glop_ok = glop ::: Pred recN --> Pred (\x -> x)
               `Using` flop_ok

{-# NOINLINE foo #-}
foo (S x) = flop (S x)
foo Z     = True

foo_ok = foo ::: Pred recN --> Pred (\x -> x)
             `Using` glop_ok


