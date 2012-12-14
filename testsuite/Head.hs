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


notnull [] = False
notnull xs = True


head_contract_1 = head ::: CF :&: Pred (not . null) --> CF

head_contract_2 = head ::: CF :&: Pred (\x -> not (null x)) --> CF

head_contract_3 = head ::: CF :&: Pred notnull --> CF 

head_broken_1 = head ::: CF --> CF
head_broken_2 = head ::: Pred (not . null) --> CF
