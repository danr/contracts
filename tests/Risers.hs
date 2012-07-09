{-# LANGUAGE BangPatterns #-}
module Risers where

import Prelude (Bool(..),otherwise,error)
import Contracts

data Nat = S Nat | Z

(<=) :: Nat -> Nat -> Bool
Z   <= Z   = True
Z   <= S _ = True
S _ <= Z   = False
S x <= S y = x <= y

risers :: [Nat] -> [[Nat]]
risers [] = []
risers [x] = [[x]]
risers (x:y:xs) = case risers (y:xs) of
    s:ss | x <= y    -> (x:s):ss
         | otherwise -> [x]:(s:ss)
    [] -> error "internal error"
--    where !(s:ss) = risers (y:xs)

null []    = True
null (_:_) = False

nonEmpty []    = False
nonEmpty (_:_) = True

not True = False
not False = True

-- Bug in lambda lifter
{-# NOINLINE (.) #-}
f . g = \x -> f (g x)

unsat_le_contr :: Statement
unsat_le_contr = (<=) ::: CF --> CF --> CF

unsat_risers_contr :: Statement
unsat_risers_contr = risers
    ::: CF :&: Pred (not . null)
    --> CF :&: Pred (not . null)
  `Using` unsat_le_contr

risersBy :: (a -> a -> Bool) -> [a] -> [[a]]
risersBy (<) [] = []
risersBy (<) [x] = [[x]]
risersBy (<) (x:y:xs) = case risersBy (<) (y:xs) of
    s:ss | x < y     -> (x:s):ss
         | otherwise -> [x]:(s:ss)
    [] -> error "internal error"

unsat_risersBy_contr :: Statement
unsat_risersBy_contr =
    risersBy ::: (CF --> CF --> CF)
             --> CF :&: Pred (not . null)
             --> CF :&: Pred (not . null)

unsat_risersBy_nonEmpty_contr :: Statement
unsat_risersBy_nonEmpty_contr =
    risersBy ::: (CF --> CF --> CF)
             --> CF :&: Pred nonEmpty
             --> CF :&: Pred nonEmpty
