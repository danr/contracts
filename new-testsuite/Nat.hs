module Nat where

import Contracts
import Prelude (Bool(..),not,id,(&&), (||))

data Nat = S Nat | Z

recB True  = True
recB False = True

recN Z     = True
recN (S x) = recN x

pl :: Nat -> Nat -> Nat
pl Z y = y
pl (S x) y = S (pl x y)

eq :: Nat -> Nat -> Bool
eq Z Z         = True
eq Z _         = False
eq (S x) Z     = False
eq (S x) (S y) = eq x y

leq :: Nat -> Nat -> Bool
leq Z x         = True
leq (S x) Z     = False
leq (S x) (S y) = leq x y

max :: Nat -> Nat -> Nat
max Z y         = y
max x Z         = x
max (S x) (S y) = S (max x y)

min :: Nat -> Nat -> Nat
min Z y         = Z
min x Z         = Z
min (S x) (S y) = S (min x y)

even :: Nat -> Bool
even Z = True
even (S Z) = False
even (S (S x)) = even x

half :: Nat -> Nat
half Z = Z
half (S Z) = Z
half (S (S x)) = S (half x)

mult :: Nat -> Nat -> Nat -> Nat
mult Z     _ acc = acc
mult (S x) y acc = mult x y (y `pl` acc)


-- qfac :: Nat -> Nat -> Nat
-- qfac Z     acc = acc
-- qfac (S x) acc = qfac x (S x * acc)

-- exp :: Nat -> Nat -> Nat
-- exp _ Z     = S Z
-- exp x (S n) = x * exp x n

-- exp_accum :: Nat -> Nat -> Nat -> Nat
-- exp_accum x Z     acc = acc
-- exp_accum x (S n) acc = exp_accum x n (x * acc)

-- double :: Nat -> Nat
-- double Z = Z
-- double (S x) = S (S (double x))

-- ack :: Nat -> Nat -> Nat
-- ack Z     n     = S n
-- ack (S m) Z     = ack m (S Z)
-- ack (S m) (S n) = ack m (ack (S m) n)


eq_ok   = eq   ::: Pred recN --> Pred recN --> Pred recB
pl_ok   = pl   ::: Pred recN --> Pred recN --> Pred recN
leq_ok  = leq  ::: Pred recN --> Pred recN --> Pred recB
max_ok  = max  ::: Pred recN --> Pred recN --> Pred recN
min_ok  = min  ::: Pred recN --> Pred recN --> Pred recN
even_ok = even ::: Pred recN --> Pred recB
half_ok = half ::: Pred recN --> Pred recN


mult_ok = mult ::: (Pred recN --> Pred recN --> Pred recN --> Pred recN)
               `Using` pl_ok


mult_broken_1 = mult ::: Pred recN --> Pred recN --> Pred recN --> Pred recN

mult_broken_2 = mult ::: Pred recN --> Pred (\z -> True)
                     --> Pred recN --> Pred recN

mult_ok_2 = mult ::: Pred recN --> Pred recN
                 --> Pred recN --> Pred (\z -> True)


pl_leq1 = pl ::: Pred recN :-> \x
              -> Pred recN :-> \y
              -> Pred recN :&: Pred (leq x)

leq_succ  = leq ::: Pred recN :-> \x ->
                    Pred (eq (S x)) --> Pred (\z -> True)



some p Z = False
some p (S x) = p x || some p x

some_ok = some ::: (Pred recN --> Pred recB) --> Pred recN --> Pred recB

dropwhile p Z = Z 
dropwhile p (S x) = if p x then (S x) else dropwhile p x

dropwhile_ok
  = dropwhile ::: (Pred recN --> Pred recB) --> Pred recN --> Pred recN

-- dropwhile does not return a satisfying suffix!
dropwhile_broken
  = dropwhile ::: (Pred recN --> Pred recB) :-> \p -> Pred recN --> Pred p

dropwhile_ok_2
  = dropwhile ::: (Pred recN --> Pred recB)
              :-> \p -> Pred recN --> Pred (\z -> recN z && sufNot p z)

sufNot p (S x) = p x
sufNot p Z     = True


g (S x) = S (S (g x))
g Z     = Z

contr_even = g ::: (Pred recN --> Pred even)


pl1 Z y = y
pl1 (S x) y = S (pl1 x y)

foo Z     = True
foo (S x) = foo x

foo_eq_refl = foo ::: Pred recN :-> \x -> Pred (\z -> (eq x x))
foo_ok = foo ::: Pred recN --> Pred recB

-- Broken because we don't know that eq is reflexive ...
plus_plus_broken
  = pl ::: (Pred recN :-> \x -> Pred recN :-> \y -> Pred (\z -> z `eq` (pl1 x y)))
       `Using` eq_ok

eq_single x = eq x x
eq_single_contr = eq_single ::: Pred recN --> Pred (\x -> x)

-- Now we add that ... 
plus_plus_ok
  = pl ::: (Pred recN :-> \x -> Pred recN :-> \y -> Pred (\z -> z `eq` (pl1 x y)))
       `Using` eq_ok
       `Using` eq_single_contr

 



