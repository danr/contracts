{-# LANGUAGE GADTs, RankNTypes #-}
module Contracts where

data Contract t where
  (:->) :: Contract a -> (a -> Contract b) -> Contract (a -> b)
  Pred  :: (a -> Bool) -> Contract a
  CF    :: Contract a
  (:&:) :: Contract a -> Contract a -> Contract a

(-->) :: Contract a -> Contract b -> Contract (a -> b)
c1 --> c2 = c1 :-> \_ -> c2

infixr 2 :->
infixr 2 -->

infixr 3 :&:

infix 1 :::

data Statement u where
   (:::) :: u -> Contract u -> Statement u
   Using :: Statement a -> Statement b -> Statement (a,b)

infixl 0 `Using`


