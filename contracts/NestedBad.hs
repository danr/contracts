module NestedBad where

import Contracts

-- w is obviously bad
w :: ()
w = undefined

-- v is not immediately bad, but it's not crash free
v :: [()]
v = [w]

v_contr :: Statement
v_contr = v ::: CF
