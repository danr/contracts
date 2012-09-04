module Mc91Int where

import Contracts
import Prelude (Bool(..),(&&),(||),Maybe(..),Either(..),fst,snd,Ordering(..))
import GHC.Base hiding (Ord(..),(==),(/=))

infixl 7  *
infixl 6  +, -

infix 4 ==, /=, <, <=, >=, >

(+)  = plusInt
(*)  = timesInt
(-)  = minusInt
(==) = eqInt
(/=) = neInt
(<)  = ltInt
(<=) = leInt
(>=) = geInt
(>)  = gtInt
div  = divInt

compare x y = if x == y then EQ
              -- NB: must be '<=' not '<' to validate the
              -- above claim about the minimal things that
              -- can be defined for an instance of Ord:
              else if x <= y then LT
              else GT

f91 :: Int -> Int
f91 n = case n <= 100 of
    True  -> f91 (f91 (n + 11))
    False -> n - 100

f91_contr = f91 ::: CF :-> \n ->
    CF :&: Pred (\r -> n <= 100 && r == 91 || n > 100 && r == n - 10)
