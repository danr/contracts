module Newtype where

import Contracts
import Prelude (Bool(..))

data Maybe a = Just a | Nothing

newtype Tri = Tri (Maybe Bool)

(&&) :: Tri -> Tri -> Tri
Tri (Just True)  && (Tri (Just True))  = Tri (Just True)
Tri (Just False) && _                  = Tri (Just False)
_                && (Tri (Just False)) = Tri (Just False)
_                && _                  = Tri Nothing

and_cf = (&&) ::: CF --> CF --> CF
