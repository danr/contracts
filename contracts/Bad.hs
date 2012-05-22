module Bad where

import Contracts

-- w is obviously bad
w :: ()
w = undefined

w2 :: ()
w2 = error "bad!"

w3 :: ()
w3 = case True of
       False -> ()

w_contr :: Statement
w_contr = w ::: CF

w2_contr :: Statement
w2_contr = w2 ::: CF

w3_contr :: Statement
w3_contr = w3 ::: CF
