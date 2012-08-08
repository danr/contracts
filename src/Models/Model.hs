{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Models.Model where

import Data.Map (Map)

-- | We call everything that is deemed a function in FOL a Symbol here
data Symbol
    = OrigFunction String
    -- ^ An original function, i.e., a function in the GHC Core
    | Constructor String
    -- ^ A constructor
    | Skolem String
    -- ^ A skolem variable
    | SkolemFunction String
    -- ^ A skolem function
    | Projection Int String
    -- ^ Projecting a constructor on some coordinate
    | Pointer String
    -- ^ A pointer
    | App
    -- ^ The app symbol
  deriving (Show,Eq,Ord)

symbolString :: Symbol -> String
symbolString s = case s of
    OrigFunction s   -> s
    Constructor s    -> s
    Skolem s         -> s
    Projection _ s   -> s
    Pointer s        -> s
    SkolemFunction s -> s
    App              -> error "symbolString on app"

isOrigFunction :: Symbol -> Bool
isOrigFunction OrigFunction{} = True
isOrigFunciton _              = False

isPointer :: Symbol -> Bool
isPointer Pointer{} = True
isPointer _         = False

data Pred = Min | CF | SkolemPredicate String
  deriving (Show,Eq,Ord)

type FunTable = [([Elt],Elt)]

type PredTable = [([Elt],Bool)]

-- | A function and its table
data Function = Function
    { function  :: Symbol
    , fun_table :: FunTable
    }

lambdaArity :: Function -> Arity
lambdaArity (Function _ ((as,_):_)) = Arity (length as)
lambdaArity _ = error "arity on a degenerate list?"

data Predicate = Predicate
    { predicate  :: Pred
    , pred_table :: PredTable
    }

newtype Elt = Elt Int
  deriving (Eq,Ord,Show,Enum,Num)

newtype DomSize = DomSize Int
  deriving (Eq,Ord,Show,Enum,Num)

newtype Arity = Arity Int
  deriving (Eq,Ord,Show,Enum,Num)

data Model = Model
    { dom_size   :: DomSize
    , functions  :: [Function]
    , predicates :: [Predicate]
    }
