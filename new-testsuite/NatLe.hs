module NatLe where

import Contracts
import Prelude (Bool(..),not,id,(&&))

data Nat = S Nat | Z

recB True  = True
recB False = True

recN Z     = True
recN (S x) = recN x

pl :: Nat -> Nat -> Nat
pl Z y = y
pl (S x) y = S (pl x y)

leq :: Nat -> Nat -> Bool
leq Z x         = True
leq (S x) Z     = False
leq (S x) (S y) = leq x y


pl_leq1 = pl ::: Pred recN :-> \x
              -> Pred recN :-> \y
              -> Pred recN :&: Pred (leq x)

-- Needs different induction hypothesis
pl_leq2_broken = pl ::: Pred recN :-> \x
                     -> Pred recN :-> \y
                     -> Pred recN :&: Pred (leq y)

pl_leq3  = pl ::: Pred recN :-> \x
               -> Pred recN :-> \y
               -> Pred (\z -> recN z && leq x z)

-- Needs different induction hypothesis
pl_leq4_broken  = pl ::: Pred recN :-> \x
                      -> Pred recN :-> \y
                      -> Pred (\z -> recN z && leq y z)

