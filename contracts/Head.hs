module Head where

import Contracts
import Prelude (Bool(..),error)

head (x:xs) = x
head []     = error "empty list"

null [] = True
null xs = False

not True = False
not False = True

f . g = \x -> f (g x)

head_contract = head ::: CF :&: Pred (not . null) --> CF
