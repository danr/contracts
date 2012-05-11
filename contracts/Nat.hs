{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}
module Contract where

import Contracts

data Nat = Zero | Succ Nat

eq :: Nat -> Nat -> Bool
eq Zero Zero = True
eq (Succ n) (Succ m) = eq n m
eq _ _ = False

idNat :: Nat -> Nat
idNat x = x

contr_idNat :: Statement
contr_idNat = idNat ::: CF :-> \x -> CF :&: (\y -> eq x y)
--   $[contr| idNat ::: ( x ) -> ( y | eq x y )]

contr_eq :: Statement
contr_eq = eq ::: CF --> CF --> CF

data Proof = QED

lem_eq_trans (Succ x) (Succ y) (Succ z) = lem_eq_trans x y z
lem_eq_trans x y z                      = QED

-- Transitivity of eq nat as a lemma
eq_trans = lem_eq_trans ::: CF :-> \x -> CF :&: Pred (\y -> eq x y)
                               :-> \y -> CF :&: Pred (\z -> eq y z)
                               :-> \z -> CF :&: Pred (\_ -> eq x z)



