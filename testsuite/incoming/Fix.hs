module Fix where

import Contracts

fix :: (a -> a) -> a
fix f = let x = f x in x

fix_contr :: Statement
fix_contr = fix ::: (CF --> CF) --> CF

-- This one is also interesting because it is a hof of depth 2
fixCbv :: ((a -> b) -> a -> b) -> a -> b
fixCbv f x = f (fixCbv f) x

fixCbv_contr :: Statement
fixCbv_contr = fixCbv ::: ((CF --> CF) --> CF --> CF) --> CF --> CF