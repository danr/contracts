module Language where

data Expr v
    = App (Expr v) [Expr v]
    | Var v
    | Con v
    | Case (Expr v) [Branch v]

data Branch v
    = Default (Expr v)
    | Pat v :-> Expr v

data Pat v = PVar v | PCon v [Pat v]


