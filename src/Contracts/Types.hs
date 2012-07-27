module Contracts.Types where

import Var
import CoreSyn

import Halo.Shared
import Halo.FOL.Abstract

import Contracts.Theory

data Contract
    = CF
    | Pred CoreExpr
    | And Contract Contract
    | Arrow Var Contract Contract

telescope :: Contract -> ([(Var,Contract)],Contract)
telescope = go []
  where
    go acc (Arrow v c1 c2) = go ((v,c1):acc) c2
    go acc c               = (reverse acc,c)

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
    | FixpointBase
    | FixpointStep
  deriving (Show,Eq,Ord)

-- | Add more dependencies to a conjecture
extendConj :: [Clause'] -> [HCCContent] -> Conjecture -> Conjecture
extendConj cls deps c = c
    { conj_clauses      = cls ++ conj_clauses c
    , conj_dependencies = deps ++ conj_dependencies c
    }

conjKindSuffix :: ConjectureKind -> String
conjKindSuffix p = case p of
    Plain        -> ""
    FixpointBase -> "_base"
    FixpointStep -> "_step"

data TopStmt = TopStmt
    { top_name      :: Var
    , top_statement :: Statement
    , top_deps      :: [HCCContent]
    }

data Statement = Statement
    { statement_expr  :: CoreExpr
    , statement_con   :: Contract
    , statement_args  :: [Var]
    , statement_using :: [Statement]
    -- ^ Stripped of tree usings
    }

-- | Don't recursively get all Usings as a tree, only as a list
stripTreeUsings :: Statement -> Statement
stripTreeUsings stmt =
    let rmUsing (Statement e c as _) = Statement e c as []
    in  stmt { statement_using = map rmUsing (statement_using stmt) }

instance Show Statement where
    show (Statement e c u as) = case as of
        [] -> rest
        xs -> "forall " ++ unwords (map show xs) ++ " . " ++ rest
      where
        rest = showExpr e ++ " ::: " ++ show c
            ++ concat [ " [using: " ++ unwords (map show u) ++ "]" | not (null u) ]

instance Show TopStmt where
    show (TopStmt n s ds) = show n ++ " = " ++ show s
                        ++ " [deps: " ++ unwords (map show ds) ++ "]"
