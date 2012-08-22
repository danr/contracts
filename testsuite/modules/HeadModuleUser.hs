module HeadModuleUser where

import Contracts
import Prelude ()
import HeadModule

head_contr = head ::: CF :&: Pred (not . null) --> CF
