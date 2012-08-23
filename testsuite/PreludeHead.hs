module PreludeHead where

import Contracts

-- An illustative contract with Prelude functions
head_contr = head ::: Pred (not . null) :&: CF --> CF

head_contr_oops = head ::: Pred (not . null) --> CF
