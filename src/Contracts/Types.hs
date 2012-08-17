{-# LANGUAGE RecordWildCards #-}
module Contracts.Types where

import Var
import CoreSyn

import Halo.Shared
import Halo.FOL.Abstract

import Data.Maybe

import Contracts.Theory

data Contract
    = CF
    | Pred CoreExpr
    | And Contract Contract
    | Arrow Var Contract Contract

-- "Natural" numbers
data Nat = S Nat | Z

one,inf :: Nat
one = S Z
inf = S inf

-- | Telescope down to a certain depth
telescope :: Nat -> Contract -> ([(Var,Contract)],Contract)
telescope = go []
  where
    go acc (S d) (Arrow v c1 c2) = go ((v,c1):acc) d c2
    go acc _     c               = (reverse acc,c)

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

-- | The translated clauses and its dependencies, and what kind of
--   conjecture it is (plain / fixpoint base / fixpoint step)
data Conjecture = Conjecture
    { conj_clauses      :: [Clause']
    , conj_dependencies :: [HCCContent]
    , conj_kind         :: ConjectureKind
    }
  deriving Show

-- | The different kinds of conjectures
data ConjectureKind
    = Plain
    | PlainSplit Int
    | FixpointBase
    | FixpointStep
    | FixpointStepSplit Int
  deriving (Show,Eq,Ord)

-- | Add more dependencies to a conjecture
extendConj :: [Clause'] -> [HCCContent] -> Conjecture -> Conjecture
extendConj cls deps c = c
    { conj_clauses      = cls ++ conj_clauses c
    , conj_dependencies = deps ++ conj_dependencies c
    }

conjKindSuffix :: ConjectureKind -> String
conjKindSuffix p = case p of
    Plain               -> ""
    PlainSplit i        -> "_" ++ show i
    FixpointBase        -> "_base"
    FixpointStep        -> "_step"
    FixpointStepSplit i -> "_step_" ++ show i

data TopStmt = TopStmt
    { top_name :: Var
    , top_stmt :: Statement
    , top_deps :: [HCCContent]
    }

data Statement
    = CoreExpr ::: Contract
    | Lambda [Var] Statement
    | Statement :=> Statement
    | Using Statement Statement

-- | Apply a list of substitutions on a Statement
substStatementList :: Statement -> [(Var,Var)] -> Statement
substStatementList s0 xs = case s0 of
    e ::: c     -> substList e xs ::: substContractList c xs
    Lambda vs s -> Lambda [fromMaybe v (lookup v xs) | v <- vs]
                          (substStatementList s xs)
    s :=> t     -> substStatementList s xs :=> substStatementList t xs
    Using s t   -> Using (substStatementList s xs) (substStatementList t xs)

instance Show Statement where
    show (e ::: c) = showExpr e ++ " ::: " ++ show c
    show (Lambda vs s) = "forall " ++ unwords (map show vs) ++ show s
    show (s :=> t) = show s ++ " :=> " ++ show t
    show (Using s t) = show s ++ " `Using` " ++ show t

instance Show TopStmt where
    show TopStmt{..} = show top_name ++ " = " ++ show top_stmt
                        ++ " [deps: " ++ unwords (map show top_deps) ++ "]"
