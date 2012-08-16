module Xu where

import Contracts
import Prelude (Bool(..))

data Nat = S Nat | Z

(+) :: Nat -> Nat -> Nat
Z     + y = y
(S x) + y = S (x + y)

plus_cf = (+) ::: CF --> CF --> CF

True && x = x
_    && _ = False

data T = T1 Bool | T2 Nat | T3 T T

noT1 :: T -> Bool
noT1 (T1 _) = False
noT1 (T2 _) = True
noT1 (T3 t1 t2) = noT1 t1 && noT1 t2

sumT :: T -> Nat
sumT (T2 a) = a
sumT (T3 t1 t2) = sumT t1 + sumT t2


{-# CONTRACT sumT :: {x | noT1 x} -> {z | True} #-}
sum_contract_oops = sumT ::: CF :&: Pred noT1 --> CF
  `Using` plus_cf

-- Counterexample:
--     c :: Xu.T
--     c =  T3 c (T3 ?1 UNR)
--
-- I wonder what ?1 is. Why isn't it printed as a T?
-- It is a T:
--
--     c :: Xu.T
--     c =  T3 c (T3 (?1 :: Xu.T) UNR)
--
-- It is min:
--
--     min(!1) <=> $true
--
-- But it's not a T1:
--     p_0_T1(!1) = !1
--
--     c_T1(!1) = !5

noT1_cf = noT1 ::: CF --> CF

sum_contract_broken = sumT ::: CF :&: Pred noT1 --> CF
  `Using` plus_cf
  `Using` noT1_cf

-- Then we get this counterexample:
--     d :: Xu.T
--     d =  T3 UNR (T3 (?1 :: Xu.T) (?1 :: Xu.T))

-- Which is actually a counterexample:
--     d = T3 UNR (T3 ?1 ?1)
-- And then looking at the tables:
--     sumT_concl d = BAD
-- Why? Because
--     sumT_concl (T3 t1 t2) = sumT_hyp t1 + sumT_hyp t2
-- But in the function table:
--     sumT_hyp UNR = Z
-- But it "should be" UNR. How can we strengthen the contract?

-- I cannot figure it out.








-- This one is easily counterexemplified by for instance T2 BAD.
-- However, it is printed as
--     i :: Xu.T
--     i =# T2 (?4 :: Xu.Nat)
-- Yeah, but BAD = ?2, so it is just some other crashing value.
sum_contract_ef = sumT ::: Pred noT1 --> CF
  `Using` plus_cf
  `Using` noT1_cf

-- Removing T1s:

{-# CONTRACT rmT1 :: {x | True} -> {z | noT1 z} #-}
rmT1 :: T -> T
rmT1 (T1 True)  = T2 Z
rmT1 (T1 False) = T2 (S Z)
rmT1 (T2 a)     = T2 a
rmT1 (T3 t1 t2) = T3 (rmT1 t1) (rmT1 t2)

rmT1_cf = rmT1 ::: CF --> CF :&: Pred noT1
