module Shrink where

import Contracts
import Prelude(Bool(..),error)

data Maybe a = Just a | Nothing

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust Nothing  = False

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = error "fromJust!"

fromJust_contr = fromJust ::: CF :&: Pred isJust --> CF

all :: (a -> Bool) -> [a] -> Bool
all p (x:xs) = p x && all p xs
all p []     = True

True  && b = b
False && _ = False

nonEmpty [] = False
nonEmpty (_:_) = True

shrink :: (a -> a -> a) -> [Maybe a] -> a
shrink op []          = error "Empty list!"
shrink op (Nothing:_) = error "Nothing!"
shrink op [Just x]    = x
shrink op (Just x:xs) = x `op` shrink op xs

shrink_lazy :: (a -> a -> a) -> [Maybe a] -> a
shrink_lazy op []     = error "Empty list!"
shrink_lazy op [x]    = fromJust x
shrink_lazy op (x:xs) = fromJust x `op` shrink_lazy op xs

shrink_cf = shrink
    ::: (CF --> CF --> CF) --> CF :&: Pred nonEmpty :&: Pred (all isJust) --> CF

broken_shrink_lazy = shrink_lazy
    ::: (CF --> CF --> CF) --> CF :&: Pred nonEmpty :&: Pred (all isJust) --> CF
