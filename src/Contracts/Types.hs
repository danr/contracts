module Contracts.Types where

import Var
import CoreSyn

import Halo.Shared

data Contract
    = CF
    | Pred CoreExpr
    | And Contract Contract
    | Arrow Var Contract Contract

instance Show Contract where
    show CF              = "CF"
    show (Pred e)        = "{" ++ showExpr e ++ "}"
    show (And e1 e2)     = show e1 ++ "&&" ++ show e2
    show (Arrow v c1 c2) = "(" ++ show v ++ ":" ++ show c1 ++ ") -> " ++ show c2

-- | Subst the contract structure
--
--   Notice that you cannot substitute the binder in the arrow
--   contract with this implementation
substContractList :: Contract -> [(Var,Var)] -> Contract
substContractList c xs = go c
  where
    go CF              = CF
    go (Pred e)        = Pred (substList e xs)
    go (And c1 c2)     = And (go c1) (go c2)
    go (Arrow x c1 c2) = Arrow x (go c1) (go c2)

-- | Subst the contract structure, see notice above
substContract :: Contract -> Var -> Var -> Contract
substContract c xold xnew = substContractList c [(xold,xnew)]

data ProofPart
    = Plain
    | FixpointBase
    | FixpointStep
  deriving (Show,Eq,Ord)

proofPartSuffix :: ProofPart -> String
proofPartSuffix p = case p of
    Plain        -> ""
    FixpointBase -> "_base"
    FixpointStep -> "_step"

data Statement = Statement
    { statement_name :: Var
    , statement_fun  :: Var
    , statement_con  :: Contract
    , statement_deps :: [Var]
    }

instance Show Statement where
    show (Statement n f c d) = show n ++ " = " ++ show f ++ " ::: " ++ show c
                               ++ " (dependencies: " ++ unwords (map show d) ++ ")"
