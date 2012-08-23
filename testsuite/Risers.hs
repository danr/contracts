{-# LANGUAGE BangPatterns #-}
module Risers where

import Prelude (Bool(..),otherwise,error,not,null,(.))
import Nat
import Contracts

risers :: [Nat] -> [[Nat]]
risers [] = []
risers [x] = [[x]]
risers (x:y:xs) = case risers (y:xs) of
    s:ss | x <= y    -> (x:s):ss
         | otherwise -> [x]:(s:ss)
    [] -> error "internal error"
--    where !(s:ss) = risers (y:xs)

nonEmpty []    = False
nonEmpty (_:_) = True

risers_cf = risers
    ::: CF :&: Pred (not . null)
    --> CF :&: Pred (not . null)
  `Using` le_cf

broken_risers_missing_le = risers
    ::: CF :&: Pred (not . null)
    --> CF :&: Pred (not . null)

broken_risers_1 = risers
    ::: CF :&: Pred (not . null) --> CF
  `Using` le_cf

broken_risers_2 = risers
    ::: CF --> CF
  `Using` le_cf

broken_risers_3 = risers
    ::: CF --> CF :&: Pred (not.null)
  `Using` le_cf

risersBy :: (a -> a -> Bool) -> [a] -> [[a]]
risersBy (<) [] = []
risersBy (<) [x] = [[x]]
risersBy (<) (x:y:xs) = case risersBy (<) (y:xs) of
    s:ss | x < y     -> (x:s):ss
         | otherwise -> [x]:(s:ss)
    [] -> error "internal error"

risersBy_cf =
    risersBy ::: (CF --> CF --> CF)
             --> CF :&: Pred (not . null)
             --> CF :&: Pred (not . null)

risersBy_nonEmpty =
    risersBy ::: (CF --> CF --> CF)
             --> CF :&: Pred nonEmpty
             --> CF :&: Pred nonEmpty

broken_risersBy_nonEmpty_1 =
    risersBy ::: (CF --> CF --> CF)
             --> CF :&: Pred nonEmpty
             --> CF

broken_risersBy_nonEmpty_2 =
    risersBy ::: (CF --> CF --> CF)
             --> CF
             --> CF
