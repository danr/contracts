{-# LANGUAGE GADTs, RankNTypes #-}
module Contracts where

data Contract t where
  (:->) :: Contract a -> (a -> Contract b) -> Contract (a -> b)
  Pred  :: (a -> Bool) -> Contract a
  CF    :: Contract a
  (:&:) :: Contract a -> Contract a -> Contract a

(-->) :: Contract a -> Contract b -> Contract (a -> b)
c1 --> c2 = c1 :-> \_ -> c2

infixr 1 :->
infixr 1 -->

infixr 2 :&:

infix 0 :::

data Statement where
   (:::) :: a -> Contract a -> Statement
