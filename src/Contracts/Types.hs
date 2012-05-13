module Contracts.Types where

import Var
import CoreSyn

import Halt.Utils

data Contract = CF
              | Pred CoreExpr
              | And Contract Contract
              | Arrow Var Contract Contract

instance Show Contract where
    show CF              = "CF"
    show (Pred e)        = "{" ++ showExpr e ++ "}"
    show (And e1 e2)     = show e1 ++ "&&" ++ show e2
    show (Arrow v c1 c2) = "(" ++ show v ++ ":" ++ show c1 ++ ") -> " ++ show c2

data Statement = Var ::: Contract
  deriving Show
