{-# LANGUAGE TupleSections,ViewPatterns #-}
{-

    We have two uses of inlining so far:

     1) When using optimisation, the contracts get splitted up into
        several entries. Example:

        concatMap_cf = (:::) concatMap a3_rke

        a3_rke = (-->) ((-->) CF CF) CF

        And we want to bring the a3_rke back here, as well as inline
            (-->) c1 c2
        to
            (:->) c1 (\ w -> c2)

        I.e. we can get core definitions from other modules that tells
        us how to create Contracts and Statements.

     2) When translating function definitions, we don't want to have
        unnecessary function around. An especially annoying pattern is:

            foo x = case x of
                K1 -> lvl_fk4
                K2 y -> foo y

            lvl_fk4 = BAD

        With endless variantions, where lvl_fk4 can get a real-world token,
        and so on.

        A function that is neither recursive nor using case is a good
        candidate for removal.

        We such top-level lifted CAFs for sharing, but we should
        try to destroy all sharing since everything is memoized in
        theorem provers anyway. So we don't want to see:

            zero = Zero
            one = Succ zero
            two = Succ one

        Always inline these. E is sometimes confused by such constants.

        Another optimisation that we *probably* want to do is
        if we have definitions like:

            reverse xs = go xs []

            go xs acc = ...

        Then, reverse should always be replaced by \ xs -> go xs []. This is
        also important for provability, for if we wanted to prove something about
        reverse, then we cannot do fixed point induction as it is right now.
        If it was always inlined to go, we could.

    Another alternative for the above is that `go` is probably defined in a
    local where clause. Then we can put a contract there:

        reverse xs = go xs []
          where
            go xs acc = ...
            go_c = go ::: CF --> CF --> CF

    Now, to make a contract for reverse, we cannot access go_c. The
    very reasonable thing to do is when we make a contract that has an
    inner contract, it is always implicitly assumed. I don't know if it is
    possible to tell from GHC core that go_c was defined locally for
    reverse, though.

    This requires partially applied contracts, and also remembering what
    functions are replaced with. But we might *need* to keep functions around
    if they are used partially applied. We cannot have a (`go` []) section
    floating around when translating to FOL. So we can only get rid of
    `reverse` if it is never used partially applied. If it is even PAP,
    then we should inline it wherever we can (where the result is not a lambda),
    and otherwise keep it as a way of flipping arguments.

-}
module Contracts.Inliner
    ( inlineProgram -- :: [CoreBind] -> ([CoreBind],InlineKit)
    , InlineKit(..)
    )
  where

import Var
import CoreSyn
import CoreFVs
import UniqSet

import Contracts.SrcRep

import Halo.Util
import Halo.Shared

import Control.Monad.Reader

import qualified Data.Map as M
import Data.Map (Map)

import Data.Function
import Data.Graph

-- | The program
type Program = Map Var CoreExpr

-- | A little mini graph of dependencies, so we can know exactly which
--   functions are recursive
type DepGraph = (Graph,Vertex -> ((),Var,[Var]),Var -> Maybe Vertex)

-- | Make the dependency graph
mkDepGraph :: [(Var,CoreExpr)] -> DepGraph
mkDepGraph prg = graphFromEdges gr
  where gr = [ ((),v,uniqSetToList (exprFreeIds e)) | (v,e) <- prg ]

-- | The next nodes from a variable (the functions it calls)
neighbours :: DepGraph -> Var -> [Var]
neighbours dg@(_,fromVertex,_) v = ns
  where (_,_,ns) = fromVertex (lookupVertex dg v)

-- | Gets the abstract Vertex from a dependency graph
lookupVertex :: DepGraph -> Var -> Vertex
lookupVertex (_,_,toVertex) v = case toVertex v of
    Just x  -> x
    Nothing -> error $ "Contracts.Inliner.lookupVertex: unbound " ++ show v

-- | Check if a function calls itself directly
selfRecursive :: DepGraph -> Var -> Bool
selfRecursive dg v = v `elem` neighbours dg v

-- | Check if a function calls itself directly or indirectly
recursive :: DepGraph -> Var -> Bool
recursive dg@(g,_,_) v = selfRecursive dg v
    || any (`pathfinder` v) (neighbours dg v)
  where
    pathfinder = path g `on` lookupVertex dg

-- | Check if an expression is directly a case, after stripping lambdas
isCaseExpr :: CoreExpr -> Bool
isCaseExpr e = case collectBinders e of
    (_,Case{}) -> True
    _          -> False

-- | The environment we carry around
type Env = (Program,DepGraph)

-- | We read from the environment
type InlineM = Reader Env

-- | Get the program
program :: InlineM Program
program = asks fst

-- | Get the graph
graph :: InlineM DepGraph
graph = asks snd

-- | The right hand side of a definition
rhs :: Var -> InlineM (Maybe CoreExpr)
rhs v = M.lookup v <$> program

-- | We inline functions that are not casers and not recursive
inlineable :: Var -> InlineM Bool
inlineable v = do
    g <- graph
    m_e <- rhs v
    case m_e of
        Just e -> return $ not (isCaseExpr e) && not (recursive g v)
        _      -> return False

extraPower,normalPower :: Bool
extraPower = True
normalPower = False

-- | Naive for now, but we could cache the results of inlining earlier functions
inline :: Bool -> CoreExpr -> InlineM CoreExpr
inline extra_power e = case e of
    Var v -> ifM ((extra_power ||) <$> inlineable v)
                 (inlineCall extra_power v []) (return e)
    App e1 e2 -> do
        let recurse = App <$> inline normalPower e1 <*> inline normalPower e2
        case collectArgs e of
            (Var f,es) | isContrPi f || isContrPred f
                       -> foldl App (Var f) <$> mapM (inline extraPower) es
            (Var f,es) -> ifM (inlineable f) (inlineCall extra_power f es) recurse
            _ -> recurse
    Case e' s t alts -> (\es -> Case es s t) <$> inline normalPower e' <*> mapM inlineAlt alts
    Lam x e'   -> Lam x <$> inline normalPower e'
    Tick t e'  -> Tick t <$> inline normalPower e'
    Cast e' c  -> (`Cast` c) <$> inline normalPower e'
    Let{}      -> return e
    Lit{}      -> return e
    Type{}     -> return e
    Coercion{} -> return e

-- | Inlines an alternative in a case expression
inlineAlt :: CoreAlt -> InlineM CoreAlt
inlineAlt (con,bs,e) = (con,bs,) <$> inline normalPower e

-- | Inlines a call f @ xs, if we know f is inlineable
inlineCall :: Bool -> Var -> [CoreExpr] -> InlineM CoreExpr
inlineCall extra_power f es = do
    m_rhs <- rhs f
    case m_rhs of
        Just (collectBinders -> (xs,e))
            -- Only apply if we have enough arguments, or function is OK to
            -- have remaining lambdas, but never inline case expressions
            | not (isCaseExpr e) && (extra_power || length es >= length xs)
            -> inline extra_power (apply xs es e)
        _ -> foldl App (Var f) <$> mapM (inline extra_power) es

apply :: [Var] -> [CoreExpr] -> CoreExpr -> CoreExpr
apply [] [] e = e
apply xs [] e = foldr Lam e xs
apply [] es e = foldl App e es
apply (x:xs) (ex:es) e = apply xs es (substExp e x ex)

-- | Inline a group of bindings
inlineBind :: CoreBind -> InlineM CoreBind
inlineBind (NonRec f e) = NonRec f <$> inline normalPower e
inlineBind (Rec fses) = Rec <$> mapM (\(f,e) -> (f,) <$> inline normalPower e) fses

-- | The little handy kit you get when running the inliner
data InlineKit = InlineKit
    { inlineExpr        :: CoreExpr -> CoreExpr
    , inlineLambdaExprs :: CoreExpr -> CoreExpr
    , varInlineable     :: Var -> Bool
    }

-- | Inlines everything in the program, and also gives you the
--   inliner for later use in translated contracts and so on
inlineProgram :: [CoreBind] -> ([CoreBind],InlineKit)
inlineProgram binds = (binds',kit)
  where
    prog  = flattenBinds binds
    depgr = mkDepGraph prog
    run m = runReader m (M.fromList prog,depgr)

    binds' = run (mapM inlineBind binds)
    kit = InlineKit
        { inlineExpr        = run . inline normalPower
        , inlineLambdaExprs = run . inline extraPower
        , varInlineable     = run . inlineable
        }
