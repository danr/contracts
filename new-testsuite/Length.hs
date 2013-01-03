module Length where

import Contracts

data Nat = S Nat | Z

pl :: Nat -> Nat -> Nat
pl Z y = y
pl (S x) y = S (pl x y)

eq :: Nat -> Nat -> Bool
eq Z Z         = True
eq Z _         = False
eq (S x) Z     = False
eq (S x) (S y) = eq x y

eqB True True   = True
eqB True False  = False
eqB False True  = False
eqB False False = False

recB True  = True
recB False = True

recN Z     = True
recN (S x) = recN x

-- Just checks the spine!
recL []     = True
recL (x:xs) = recL xs

len []     = Z
len (x:xs) = S (len xs)

isZero Z = True
isZero _ = False

eq_ok_1  = eq  ::: Pred recN --> Pred recN --> Pred recB
eq_refl  = eq  ::: Pred recN :-> \x -> Pred (\y -> x `eq` y) --> Pred (\z -> z `eqB` True)

len_ok_1 = len ::: Pred recL --> Pred recN

pl_ok_1  = pl ::: Pred recN --> Pred recN --> Pred recN

len_broken_1 = len ::: Pred (const True) --> Pred recN
len_broken_2 = len ::: Pred recL --> Pred (\z -> recN z && isZero z)

-- Append
app [] ys = ys
app (x:xs) ys = x : (app xs ys)

app_ok_1 = app ::: Pred recL --> Pred recL --> Pred recL

{-# NOINLINE eq_single_ni #-}
eq_single_ni Z = eq Z Z
eq_single_ni (S x) = eq (S x) (S x)

eq_single_contr_ni = eq_single_ni ::: Pred recN --> Pred (\x -> x)


eq_single Z = eq Z Z
eq_single (S x) = eq (S x) (S x)

eq_single_contr_broken = eq_single ::: Pred recN --> Pred (\x -> x)

{-
-- This is REALLY UNSAT, but it takes equinox a long time to find it! 
app_ok_2
  = app ::: (Pred recL :-> \xs -> Pred recL :-> \ys -> Pred (\z -> recL z && len z `eq` pl (len xs) (len ys)))
        `Using` len_ok_1
        `Using` pl_ok_1
        `Using` eq_ok_1
        `Using` eq_single_contr

-- This is the same as above but without reflexivity of equality
app_broken_1
  = app ::: (Pred recL :-> \xs -> Pred recL :-> \ys -> Pred (\z -> recL z && len z `eq` pl (len xs) (len ys)))
        `Using` len_ok_1
        `Using` pl_ok_1
        `Using` eq_ok_1


-- This one has reflexivity but the min's dont work out because we
-- never have min(eq_single_ni x) though we do get min(eq x x)! 
app_broken_2
  = app ::: (Pred recL :-> \xs -> Pred recL :-> \ys -> Pred (\z -> recL z && len z `eq` pl (len xs) (len ys)))
        `Using` len_ok_1
        `Using` pl_ok_1
        `Using` eq_ok_1
        `Using` eq_single_contr_ni 


-}
{- 
-- Reverse
rev [] acc = acc
rev (x:xs) acc = rev xs (x:acc)



rev_ok_1 = rev ::: Pred recL :-> \xs
                -> Pred recL :-> \acc
                -> Pred recL 

rev_ok_2 = rev ::: (Pred recL :-> \xs
                -> Pred recL :-> \acc
                -> Pred (\z -> recL z && len z `eq` pl (len xs) (len acc)))
               `Using` len_ok_1



-}