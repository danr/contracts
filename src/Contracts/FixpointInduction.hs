{-

    Fixed point induction
    =====================

    Say we have a group of definitions:

        f xs = ... f ... g ...
        g ys = ... f ... g ...

    For each function (f,g), example below f, we need to do this for
    base case:

        f_unr xs = UNR
        g_f_unr ys     = ... f_unr ... g_f_unr ...

    and this for step case:

        f_conc xs    = ... f_hyp ... g_f_step ...
        g_f_step ys  = ... f_hyp ... g_f_step ...

    Example,

        even Z     = True
        even (S x) = odd x

        odd Z     = False
        odd (S x) = even x

    Now, doing fpi on even in the step case becomes:

        even_conc Z     = True
        even_conc (S x) = odd_even_step x

        odd_even_step Z     = False
        odd_even_step (S x) = even_hyp x

    We call @even@ the /focused/ function, and @odd@ the /friend/ function.

    Obviously, the focused function is renamed thrice: to base,
    hypothesis and conclusion, and the friend functions are only
    renamed to base and step.

    Remember: even_hyp will become a free variable in these generated
    expressions, so better make an empty Subtheory for f_hyp or else the
    subtheory trimmer will complain on lost function definition

-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Contracts.FixpointInduction
    ( fixpointCoreProgram
    , FixInfo
    , fpiApplicable
    , fpiFocusName
    , fpiFriendName
    , fpiGetSubstList
    , fpiHypVars
    , fpiFixHypArityMap
    , focusToFriend
    , FocusCase(..)
    , FriendCase(..)
    ) where

import CoreSyn
import Var
import Name hiding (varName)
import UniqSupply

import Halo.Shared
import Halo.PrimCon
import Halo.Util
import Halo.Monad (ArityMap)

import Control.Monad

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Monoid

-- | The cases for the focused function of fixed point induction
data FocusCase
    = ConstantUNR
    -- ^ Base, makes the current function a constant UNR function
    | Hyp
    -- ^ Hypothesis, has no implementation
    | Concl
    -- ^ Conclusion, implementation where recursive calls go to hyp
  deriving (Eq,Ord,Show)

-- | The cases for the friends of the focused function
data FriendCase
    = Base
    -- ^ Base, calls the constant UNR function
    | Step
    -- ^ Step, calls the hypothesis function
  deriving (Eq,Ord,Show)

newtype FixInfo = FixInfo
    ((Map Var (Map FocusCase Var,Map (Var,FriendCase) Var))
    -- ^ Given a function name, gives you access to what it is called
    --   in the focal cases, and what friendly variables are called
    --   in the friendly cases.
    ,[Var])
    -- ^ Names of hypothesis functions
  deriving Monoid

-- | Gets then vars of all hypothesis functions
fpiHypVars :: FixInfo -> [Var]
fpiHypVars (FixInfo (_,vs)) = vs

-- | True if you can do fixed point induction on this function
fpiApplicable :: FixInfo -> Var -> Bool
fpiApplicable (FixInfo (m,_)) f = M.member f m

fatalVar :: String -> Var -> a
fatalVar s v = error $
    s ++ ": Internal error, cannot do fixed point induction on " ++
    showOutputable v ++ " (be sure to query fpiApplicable first)"

-- | @fpiFocusName fix_info f case@ looks up what the function @f@
--   is called in this case
fpiFocusName :: FixInfo -> Var -> FocusCase -> Var
fpiFocusName (FixInfo (m,_)) f fcase = case M.lookup f m of
    Nothing -> fatalVar "fpiFocusName" f
    Just (m',_) -> fromMaybe err (M.lookup fcase m')
      where
        err = error $ "fpiFocusName: Internal error, cannot find " ++
                showOutputable f ++ " in focal case " ++ show fcase

-- | @fpiFriendName fix_info f case g@ looks up what the function @g@
--   is called in this case
fpiFriendName :: FixInfo -> Var -> FriendCase -> Var -> Var
fpiFriendName (FixInfo (m,_)) f gcase g = case M.lookup f m of
    Nothing -> fatalVar "fpiFriendName" f
    Just (_,m') -> fromMaybe g (M.lookup (g,gcase) m')

-- | Gets the entire substitution list on this particular case
fpiGetSubstList :: FixInfo -> Var -> FocusCase -> [(Var,Var)]
fpiGetSubstList fi@(FixInfo (m,_)) f fcase = case M.lookup f m of
    Nothing -> fatalVar "fpiGetSubstList" f
    Just (_focus,friends) -> (f,fpiFocusName fi f fcase):
        [ (g,g') | ((g,c),g') <- M.toList friends , focusToFriend fcase == c ]

focusToFriend :: FocusCase -> FriendCase
focusToFriend ConstantUNR = Base
focusToFriend _           = Step

-- | We need to add the arities to all the hypothesis functions,
--   since they have no implementation.
fpiFixHypArityMap :: FixInfo -> ArityMap -> ArityMap
fpiFixHypArityMap (FixInfo (m,_)) am = M.union am $ M.fromList
    [ (f_hyp,arity)
    | (f,(focus_map,_)) <- M.toList m
    , (Hyp,f_hyp) <- M.toList focus_map
    , Just arity <- [M.lookup f am]
    ]


-- | Lifts all the core binds to fixed point induction
fixpointCoreProgram :: [CoreBind] -> UniqSM ([CoreBind],FixInfo)
fixpointCoreProgram = liftM mconcat . mapM fixateBind

-- | Fixates everything recursive, adds these new functions to the
--   core binds
fixateBind :: CoreBind -> UniqSM ([CoreBind],FixInfo)
fixateBind b = case b of
    NonRec f e
        -- Function does not seem to be recursive
        | f `notElem` exprFVs e -> return ([b],mempty)

    NonRec f e -> fixateGroup [(f,e)]
    Rec fses -> fixateGroup fses

-- | Fixates every function in this group, also returns
--   the group we started with
fixateGroup :: [(Var,CoreExpr)] -> UniqSM ([CoreBind],FixInfo)
fixateGroup g = do

    let with_inspect_cursor :: ((Var,CoreExpr),[(Var,CoreExpr)])
                            -> UniqSM ([CoreBind],FixInfo)
        with_inspect_cursor ((f,e),g') = fixate f e g'

    g_fixated' <- mapM with_inspect_cursor (inspect g)

    -- Returns all the fixated groups but also the input group
    return $ ([mkCoreBind g],mempty) `mappend` mconcat g_fixated'

-- | Fixate a function, given its name and rhs, and the other
--   functions in its recursive group.
fixate :: Var -> CoreExpr -> [(Var,CoreExpr)]
       -> UniqSM ([CoreBind],FixInfo)
fixate f e friends = do

    -- Base -------------------------------------------------------------------

    -- make a constant UNR function and make base friend cases

    f_base <- newVarWithLabel f "_unr"

    let e_base :: CoreExpr
        e_base = uncurry mkLams . second (const (primExpr UNR)) . collectBinders $ e

    let lbl_base = '_':varOccStr f ++ "_base"

    friends_base <- sequence
        [ (,,) g g_e <$> newVarWithLabel g lbl_base
        | (g,g_e) <- friends
        ]

    let subst_base :: [(Var,Var)]
        subst_base = (f,f_base):[ (g,g_base) | (g,_,g_base) <- friends_base ]

        friends_base' :: [(Var,CoreExpr)]
        friends_base' = [ (g_base,substGblIds subst_base g_e)
                        | (_,g_e,g_base) <- friends_base ]

        base_bind :: CoreBind
        base_bind = mkCoreBind ((f_base,e_base):friends_base')

    -- Step -------------------------------------------------------------------

    -- make a new names for f_hyp and f_concl, and make friend step cases

    [f_hyp,f_concl] <- mapM (newVarWithLabel f) ["_hyp","_concl"]

    let lbl_step = '_':varOccStr f ++ "_step"

    friends_step <- sequence
        [ (,,) g g_e <$> newVarWithLabel g lbl_step
        | (g,g_e) <- friends
        ]

    let subst_step :: [(Var,Var)]
        subst_step = (f,f_hyp):[ (g,g_step) | (g,_,g_step) <- friends_step ]

        e_step :: CoreExpr
        e_step = substGblIds subst_step e

        friends_step' :: [(Var,CoreExpr)]
        friends_step' = [ (g_step,substGblIds subst_step g_e)
                        | (_,g_e,g_step) <- friends_step ]

        step_bind :: CoreBind
        step_bind = mkCoreBind ((f_concl,e_step):friends_step')

    -- Wrap it all up, and return ---------------------------------------------

    let newFixInfo = FixInfo
            (M.singleton f
                (M.fromList
                    [(ConstantUNR,f_base)
                    ,(Hyp        ,f_hyp)
                    ,(Concl      ,f_concl)
                    ]
                ,M.fromList $
                    [ ((g,Base),g_base) | (g,_,g_base) <- friends_base ] ++
                    [ ((g,Step),g_step) | (g,_,g_step) <- friends_step ]
                )
            ,[f_hyp])

    return ([base_bind,step_bind],newFixInfo)

-- | Makes a new variable from an existing one, but append a label
newVarWithLabel :: Var -> String -> UniqSM Var
newVarWithLabel var lbl = do
    unq <- getUniqueM
    let name' = mkSystemName unq (mkVarOcc (varOccStr var ++ lbl))
        var'  = var `setVarUnique` unq `setVarName` name'
    return var'

-- | Gets the String in the underlying OccName in a Var
varOccStr :: Var -> String
varOccStr = occNameString . nameOccName . varName
