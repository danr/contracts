{-

    A test run of the printer, with some hard-coded types using the
    ProtoType instance for Typelike.

-}
module Contracts.Models.Test where

import Contracts.Models.Model
import Contracts.Models.Show
import Contracts.Models.ProtoType
import Contracts.Models.ParadoxParser

import System.Environment

import qualified Data.Map as M
import Data.Map (Map)

nat = Prim Nat
bool = Prim Bool

env :: Map String ProtoType
env = M.fromList
    [ ("x",nat)
    , ("y",nat)
    , ("z",nat)
    , ("u",nat)
    , ("w",nat)
    , ("t",nat)
    , ("j",nat)
    , ("k",nat)
    , ("BAD",Any)
    , ("UNR",Any)
    , ("True",bool)
    , ("False",bool)
    , ("Succ",nat :-> nat)
    , ("Zero",nat)
    , ("-_hyp",nat :-> nat :-> nat)
    , ("-_concl",nat :-> nat :-> nat)
    , ("<=",nat :-> nat :-> bool)
    ]

main = do
    [f] <- getArgs
    m_txt <- readFile f
    let m = parseParadoxModel m_txt
    putStrLn (showModel env m)


