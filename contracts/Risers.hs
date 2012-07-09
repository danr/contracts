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

-- risers_contr :: Statement
-- risers_contr = risersBy ::: (CF :-> \x -> CF :-> \y -> CF)
--                         :-> \le -> CF :&: Pred (\xs -> full xs)
--                         :-> \xs -> CF :&: Pred (\ys -> full ys)

risers_contr :: Statement
risers_contr = risersBy ::: (CF --> CF --> CF)
                          --> CF :&: Pred full
                          --> CF :&: Pred full
