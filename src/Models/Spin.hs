{-

    Spinning around the app table, in Agda:

    spin : {n : Arity} -> Elt -> (Vec Elt 2 -> Elt) -> Vec Elt n -> Elt
    spin e app []        = e
    spin f app (x :: xs) = spin (app (f :: x :: [])) xs

    What's a function table in Haskell? Start with ([Elt] -> Elt), but
    memo it with a Map over the domain.

-}
module Models.Spin where

import Control.Monad
import qualified Data.Map as M
import Data.Map (Map)

import Models.Model

-- | Gives the domain of a given size
domain :: DomSize -> [Elt]
domain (DomSize d) = [1..Elt d]

-- | All arguments for a function of a given arity and domain size
arguments :: DomSize -> Arity -> [[Elt]]
arguments d (Arity n) = replicateM n (domain d)

-- | Spins around the app table, under a given arity and domain size.
--   Starts from one domain element, and uses the app table repeatedly.
spinner :: DomSize -> Arity -> Elt -> (Elt -> Elt -> Elt) -> FunTable
spinner d n e app = map ((,) `ap` spun) (arguments d n)
  where
    spun :: [Elt] -> Elt
    spun = spin e

    spin :: Elt -> [Elt] -> Elt
    spin f (x:xs) = spin (app f x) xs
    spin e []     = e
