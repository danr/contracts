-- A prototype type
module Models.ProtoType where

import Models.Show (Typelike(..))
import Models.Model

import Data.Maybe
import Control.Arrow (first)

infixr 7 :->

data Prim = Bool | Nat | List ProtoType
  deriving Eq

instance Show Prim where
    show Bool     = "Bool"
    show Nat      = "Nat"
    show (List t) = "[" ++ show t ++ "]"

data ProtoType
    = Prim Prim
    | ProtoType :-> ProtoType
    | Var String
    | Any
  deriving Eq

instance Show ProtoType where
    showsPrec _ (Prim p)  = shows p
    showsPrec _ (Var s)   = showString s
    showsPrec _ Any       = showString "Any"
    showsPrec d (t :-> u) = showParen (d >= 8) $
        showsPrec 8 t . showString " -> " . showsPrec 7 u

instance Typelike ProtoType where
    eqTy = (==)
    showTy = show

    peel (Arity 0) t = ([],t)
    peel n (t :-> u) = first (t:) (peel (pred n) u)
    peel n t         = ([],t)

    arity (u :-> v) = succ (arity v)
    arity _         = Arity 0

    Any         `lg` _           = True
    Var a       `lg` Var b       = a == b
    Var a       `lg` _           = True
    Prim p      `lg` Prim q      = p `lgp` q
    (t1 :-> t2) `lg` (u1 :-> u2) = (t1 `lg` u1) && (t2 `lg` u2)
    _           `lg` _           = False

    unifySubst r s = let si = getSubst r s in \t -> subst si t

lgp :: Prim -> Prim -> Bool
List a `lgp` List b = a `lg` b
u      `lgp` v      = u == v

getSubst :: ProtoType -> ProtoType -> [(String,ProtoType)]
getSubst (Var a)     t           = [(a,t)]
getSubst (Prim p)    (Prim q)    = getSubstPrim p q
getSubst (t1 :-> t2) (u1 :-> u2) = getSubst t1 u1 ++ getSubst t2 u2
getSubst Any         t           = []
getSubst u v = error $ "getSubst on " ++ show u ++ " and " ++ show v

getSubstPrim :: Prim -> Prim -> [(String,ProtoType)]
getSubstPrim (List u) (List v) = getSubst u v
getSubstPrim u v
    | u == v    = []
    | otherwise = error $ "getSubstPrim on different primitives: "
                            ++ show u ++ " and " ++ show v

subst :: [(String,ProtoType)] -> ProtoType -> ProtoType
subst s = go
  where
    go :: ProtoType -> ProtoType
    go t@(Var a) = fromMaybe t (lookup a s)
    go Any       = Any
    go (Prim u)  = Prim (go' u)
    go (u :-> v) = go u :-> go v

    go' :: Prim -> Prim
    go' (List u) = List (go u)
    go' k        = k

