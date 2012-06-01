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

data Statement = Statement
    { statement_name :: Var
    , statement_fun  :: Var
    , statement_con  :: Contract
    , statement_deps :: [Var]
    }

instance Show Statement where
    show (Statement n f c d) = show n ++ " = " ++ show f ++ " ::: " ++ show c
                               ++ " (dependencies: " ++ unwords (map show d) ++ ")"
