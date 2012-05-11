module Bad where

import Contracts

-- w is obviously bad
w :: ()
w = undefined

w_contr :: Statement
w_contr = w ::: CF