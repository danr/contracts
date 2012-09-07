module Example where

import Contracts

data T a b = T a b | K a

swap (T x y) = T y x
swap (K x)   = K x

swap_cf = swap ::: CF --> CF
