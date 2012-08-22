{-# LANGUAGE GADTs, RankNTypes, EmptyDataDecls #-}
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

data Using a b
data Assuming a b
data All a b

data Statement a where
    (:::) :: a -> Contract a -> Statement a
    (:=>) :: Statement a -> Statement b -> Statement (Assuming a b)
    Using :: Statement a -> Statement b -> Statement (Using a b)
    All   :: (a -> Statement b) -> Statement (All a b)

infixr 0 :=>
infixl 0 `Using`
