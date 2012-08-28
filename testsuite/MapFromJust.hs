module MapFromJust where

import Contracts
import Prelude (Bool(..),error,($),(&&))
import List (map,all,all_cf)
import Data.Maybe (Maybe(..),fromJust)

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust Nothing  = False

map_pred_helper_broken = All $ \p -> All $ \f ->
    p ::: CF --> CF :=>
    f ::: CF :&: Pred p --> CF :=>
    map f ::: CF :&: Pred (all p) --> CF

map_pred_helper_help_broken = (All $ \p -> All $ \f ->
    p ::: CF --> CF :=>
    f ::: CF :&: Pred p --> CF :=>
    map f ::: CF :&: Pred (all p) --> CF)
  `Using` isJust ::: CF --> CF
  `Using` fromJust ::: CF :&: Pred isJust --> CF

map_pred_helper_alt_broken = All $ \p ->
    p ::: CF --> CF :=>
    map ::: (CF :&: Pred p --> CF) --> CF :&: Pred (all p) --> CF

map_pred_helper_alt_help_broken = (All $ \p ->
    p ::: CF --> CF :=>
    map ::: (CF :&: Pred p --> CF) --> CF :&: Pred (all p) --> CF)
  `Using` isJust ::: CF --> CF
  `Using` fromJust ::: CF :&: Pred isJust --> CF

map_fromJust_cf_broken = map fromJust ::: CF :&: Pred (all isJust) --> CF

map_fromJust_cf_all_broken = map fromJust ::: CF :&: Pred (all isJust) --> CF
  `Using` all_cf
  `Using` (isJust ::: CF :&: Pred isJust --> CF)


