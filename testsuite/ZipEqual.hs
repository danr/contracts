module ZipEqual where

import Contracts
import Prelude (Bool(..),(||),not,error)
import Nat
import List

-- From ghc/utils/Util.lhs
zipEqual []     []     = []
zipEqual (a:as) (b:bs) = (a,b) : zipEqual as bs
zipEqual _      _      = error "zipEqual: unequal lists"


zipEqual_broken = zipEqual ::: (CF --> CF --> CF)

zipEqual_cf_weaker = zipEqual
    ::: (CF :-> \xs ->
        CF :&: Pred (\ys -> length ys == length xs) -->
        CF)

zipEqual_cf = zipEqual
    ::: (CF :-> \xs ->
        CF :&: Pred (\ys -> length ys == length xs) -->
        CF :&: Pred (\zs -> length zs == length xs))

