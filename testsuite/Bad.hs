module Bad where

import Contracts

w :: ()
w = error "bad!"

w2 :: ()
w2 = case True of
       False -> ()

w3 :: ()
w3 = undefined

w_ef :: Statement ()
w_ef = w ::: CF

w2_ef :: Statement ()
w2_ef = w2 ::: CF

w3_ef :: Statement ()
w3_ef = w2 ::: CF

w_list_ef :: Statement [()]
w_list_ef = [w] ::: CF

w_list_ef_2 :: Statement [()]
w_list_ef_2 = [(),(),(),w,(),()] ::: CF
