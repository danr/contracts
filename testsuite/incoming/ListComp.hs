{-# LANGUAGE ParallelListComp #-}
module ListComp where

import Prelude()
import Contracts

{- The inliner needs to be a little bit more liberal to accept these,
   for the statements it needs to do this inlining:

    Inlineable: True
    Before inlining:
    map_cf = map ::: (CF --> CF) --> CF --> CF
    After inlining:
    map_cf = (\ f xs -> poly_ds_lgb f xs)
         ::: (CF --> CF) --> CF --> CF
-}
map f xs     = [ f x | x <- xs ]
map_cf       = map    ::: (CF --> CF) --> CF --> CF

-- This one works fine too
filter p xs  = [ x | x <- xs, p x ]
filter_cf    = filter ::: (CF --> CF) --> CF --> CF

-- Prod needs a lemma about append or concat that's buried
-- in the functions that are generated here so we are definitely
-- out of luck here. One possible solution would be if we could
-- translate list comprehensions into concatMap/return/guard or
-- something and use lemmas for them.
prod xs ys   = [ (x,y) | x <- xs, y <- ys ]
prod_cf = prod   ::: CF --> CF --> CF

-- This one breaks when /splitting/ the goal, the reason is that
-- the skolems for the inlined lambda does not match up correctly.
-- This should be fixed
zip xs ys    = [ (x,y) | x <- xs | y <- ys ]
zip_cf       = zip    ::: CF --> CF --> CF
