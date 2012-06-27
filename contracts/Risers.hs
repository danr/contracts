{-# LANGUAGE BangPatterns #-}
module Risers where

import Prelude (Bool(..),otherwise)
import Contracts

risersBy :: (a -> a -> Bool) -> [a] -> [[a]]
risersBy (<=) [] = []
risersBy (<=) [x] = [[x]]
risersBy (<=) (x:y:xs)
  | x <= y    = (x:s):ss
  | otherwise = [x]:(s:ss)
    where !(s:ss) = risersBy (<=) (y:xs)

full [] = False
full _  = True

risers_contr :: Statement
risers_contr = risersBy ::: (CF :-> \x -> CF :-> \y -> CF)
                         :-> \le -> CF :&: Pred full
                         :-> \xs -> CF :&: Pred full

risers_contr_2 :: Statement
risers_contr_2 = risersBy ::: (CF :-> \x -> CF :-> \y -> CF)
                          :-> \le -> CF
                          :-> \xs -> CF :&: Pred full
