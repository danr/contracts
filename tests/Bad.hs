module Bad where

import Contracts

w :: ()
w = error "bad!"

w2 :: ()
w2 = case True of
       False -> ()

sat_w = w ::: CF

sat_w2 = w2 ::: CF
