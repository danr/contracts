{-# LANGUAGE RecordWildCards #-}
{-

    The internal representations of contracts and statements,
    and various related helper functions.

-}
module Contracts.Types where

import Var
import CoreSyn

import Halo.Shared
import Halo.Util
import Halo.FOL.Abstract
import Halo.FreeTyCons
import Halo.Subtheory
import Halo.Class

import Data.Maybe
import Data.List

import Contracts.Theory

-- | The contract data type
data Contract
    = CF
    | Pred CoreExpr
    | And Contract Contract
    | Arrow Var Contract Contract

-- | "Natural" numbers
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

-- | File name suffix for a given kind of conjecture
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
    -- ^ Cached dependencies
    }

-- | Collapse all bindings
mkAll :: Var -> Statement -> Statement
mkAll x (All xs s) = All (x:xs) s
mkAll x s          = All [x]    s

-- | The statement data type
data Statement
    = CoreExpr ::: Contract
    | All [Var] Statement
    | Statement :=> Statement
    | Using Statement Statement

-- | Apply a list of substitutions on a Statement
substStatementList :: Statement -> [(Var,Var)] -> Statement
substStatementList s0 xs = case s0 of
    e ::: c   -> substList e xs ::: substContractList c xs
    All vs s  -> All [fromMaybe v (lookup v xs) | v <- vs]
                     (substStatementList s xs)
    s :=> t   -> substStatementList s xs :=> substStatementList t xs
    Using s t -> Using (substStatementList s xs) (substStatementList t xs)

instance Show Statement where
    show (e ::: c)   = showExpr e ++ " ::: " ++ show c
    show (All vs s)  = "forall " ++ unwords (map show vs) ++ show s
    show (s :=> t)   = show s ++ " :=> " ++ show t
    show (Using s t) = show s ++ " `Using` " ++ show t

instance Show TopStmt where
    show TopStmt{..} = show top_name ++ " = " ++ show top_stmt
                        ++ " [deps: " ++ unwords (map show top_deps) ++ "]"

-- | Removes nested right branches of Usings
unTreeStmt :: Statement -> Statement
unTreeStmt = go False
  where
    go i (Using s t)
        | i         = go i s
        | otherwise = go i s `Using` go True t
    go i (s :=> t)  = go i s :=> go i t
    go i (All vs s) = All vs (go i s)
    go _ (e ::: c)  = e ::: c

-- | Get the top contract for a statement
stmtContract :: Statement -> (CoreExpr,Contract)
stmtContract (e ::: c)   = (e,c)
stmtContract (_ :=> t)   = stmtContract t
stmtContract (All _ s)   = stmtContract s
stmtContract (Using s _) = stmtContract s

-- | The top function and arguments of an expression
--   (with types stripped off)
topExpr :: CoreExpr -> Maybe (Var,[CoreExpr])
topExpr e0 = case e0 of
    e@App{}  -> case second trimTyArgs (collectArgs e) of
        (Var x,es) -> Just (x,es)
        _          -> Nothing
    Var x    -> Just (x,[])
    Lam _ e  -> topExpr e
    Cast e _ -> topExpr e
    Tick _ e -> topExpr e
    _        -> Nothing

-- | Top variable of a statement, for fpi
topStmtVar :: Statement -> Maybe Var
topStmtVar = fmap fst . topStmtExpr

-- | Top of a statement, the function + its arguments
topStmtExpr :: Statement -> Maybe (Var,[CoreExpr])
topStmtExpr = topExpr . fst . stmtContract

-- | Dependencies of an expression
exprDeps :: CoreExpr -> [HCCContent]
exprDeps e = foldl1 union $
    map ($e)
        [functions . exprFVs
        ,datas . freeTyCons
        ,dictDeps]

-- | Dependencies of a statement
stmtDeps :: Statement -> [HCCContent]
stmtDeps (e ::: c)   = exprDeps e `union` contrDeps c
stmtDeps (All vs s)  = stmtDeps s \\ functions vs
stmtDeps (s :=> t)   = stmtDeps s `union` stmtDeps t
stmtDeps (Using s t) = stmtDeps s `union` stmtDeps t

-- | Is this the top of a statement?
isTop :: Statement -> Bool
isTop (_ ::: _) = True
isTop (All _ s) = isTop s
isTop _         = False

-- | Does this content appear in an assumption in a statement?
inAssumption :: HCCContent -> Statement -> Bool
inAssumption c s0 = case s0 of
    s :=> t   -> c `elem` stmtDeps s || (not (isTop t) && inAssumption c t)
    All _ s   -> inAssumption c s
    Using s _ -> inAssumption c s
    _ ::: _   -> False

-- | Does this content appear in a predicate in the top contract?
inTopPredicate :: HCCContent -> Statement -> Bool
inTopPredicate c = (c `elem`) . contrDeps . snd . stmtContract

-- | Dependencies of a contract
contrDeps :: Contract -> [HCCContent]
contrDeps CF              = []
contrDeps (Pred e)        = exprDeps e
contrDeps (And c1 c2)     = contrDeps c1 `union` contrDeps c2
contrDeps (Arrow x c1 c2) = delete (Function x) (contrDeps c1 `union` contrDeps c2)

