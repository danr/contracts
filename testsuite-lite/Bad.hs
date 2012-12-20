module Bad where

import Contracts

data T = T

recTrue :: T -> Bool
recTrue T = True

f1 :: T
f1 = error "bad!"

f2 :: T
f2 = case True of False -> T


f3 :: T
f3 = case True of True -> T

f1_broken = f1 ::: Pred recTrue
f2_broken = f2 ::: Pred recTrue
f3_ok     = f3 ::: Pred recTrue

recTrueList []     = True
recTrueList (x:xs) = recTrue x && recTrueList xs

f4 = [f1,f2]
f4_broken = f4 ::: Pred recTrueList

f5 = [f3,f3]
f5_ok = f5 ::: Pred recTrueList

