module MapFromJust where

import Contracts
import Prelude (Bool(..),error,($),(&&))
import List (map,all,all_cf)
import Data.Maybe (Maybe(..),fromJust)

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust Nothing  = False

map_pred_helper = All $ \p -> All $ \f ->
    f ::: CF :&: Pred p --> CF :=>
    map f ::: CF :&: Pred (all p) --> CF

map_pred_helper_alt = All $ \p ->
    map ::: (CF :&: Pred p --> CF) --> CF :&: Pred (all p) --> CF

map_fromJust_cf = map fromJust ::: CF :&: Pred (all isJust) --> CF
  `Using` map_pred_helper

map_fromJust_cf_alt = map fromJust ::: CF :&: Pred (all isJust) --> CF
  `Using` map_pred_helper_alt

-- Proving it directly doesn't work
map_fromJust_cf_broken = map fromJust ::: CF :&: Pred (all isJust) --> CF

-- Even if you assume some CF for all and isJust
map_fromJust_cf_all_broken = map fromJust ::: CF :&: Pred (all isJust) --> CF
  `Using` all_cf
  `Using` (isJust ::: CF :&: Pred isJust --> CF)

