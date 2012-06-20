module Counterexample where

import Contracts
import Prelude (Bool(..),undefined)

not True  = False
not False = True

True  && b = b
False && _ = False


loop = loop

f :: Bool -> (Bool -> Bool -> Bool) -> Bool
f b h | h True b && h b True && not (h False False)
         = if h True loop && h loop True then undefined else True
f b h = True

countercontract = f ::: CF --> (CF --> CF --> CF) --> CF