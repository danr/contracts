{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-

    A prover-agnostic representation of the different components of a model

-}
module Contracts.Models.Model where

-- | We call everything that is deemed a function in FOL a Symbol here
data Symbol
    = Fun String
    -- ^ A function from GHC core or an unspun pointer
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

newtype Arity = Arity { getArity :: Int }
  deriving (Eq,Ord,Show,Enum,Num)

data Model = Model
    { dom_size   :: DomSize
    , functions  :: [Function]
    , predicates :: [Predicate]
    }
