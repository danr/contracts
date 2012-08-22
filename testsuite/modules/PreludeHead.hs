module PreludeHead where

import Contracts

-- An illustative contract with Prelude functions
head_contr :: Statement ([a] -> a)
head_contr = head ::: Pred (not . null) --> CF
