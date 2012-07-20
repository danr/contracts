module Bad where

import Contracts

w :: ()
w = error "bad!"

w2 :: ()
w2 = case True of
       False -> ()

broken_w :: Statement
broken_w = w ::: CF

broken_w2 :: Statement
broken_w2 = w2 ::: CF
