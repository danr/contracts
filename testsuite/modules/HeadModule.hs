module HeadModule where

import Prelude(Bool(..))

{-# NOINLINE head #-}
head (x:xs) = x

null []     = True
null (x:xs) = False

f . g = \x -> f (g x)

not True  = False
not False = True
