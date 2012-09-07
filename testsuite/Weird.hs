module Weird where

import Contracts
import Prelude(Bool(..),Maybe(..),error,undefined,(&&),(||),not,(.),null,flip,uncurry,fst,snd,head,tail)
import Data.Maybe (fromJust)

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust Nothing  = False

swap (x,y) = (y,x)

unJustPair (x,y) = (fromJust x,fromJust y)

bothJust (x,y)   = isJust x && isJust y
bothJust' (y,x)   = isJust x && isJust y

unJustPair_cf_alt_swap =
    unJustPair ::: CF :&: Pred bothJust :&: Pred (bothJust') --> CF

unJustPair_cf_comp =
    unJustPair ::: CF :&: Pred bothJust :&: Pred (bothJust . swap) --> CF

    {-
data Id a = Id { unId :: a }

unIdPair (x,y) = (unId x,unId y)

isId (Id x) = True

bothId (x,y) = isId x && isId y

unIdPair_cf = unIdPair
     ::: (CF :&: Pred bothId :&: Pred (bothId . swap)) --> CF

     -}

{-
swap = uncurry (flip (,))

unJustPair_cf_broken   = unJustPair ::: CF :&: Pred bothJust --> CF

unJustPair_cf          = unJustPair ::: CF :&: Pred bothJust :&: Pred (bothJust . swap) --> CF


unJustPair_cf_projs    = unJustPair ::: CF :&: Pred (isJust . fst) :&: Pred (isJust . snd) --> CF
-}
