-- Dimitrio's parallel or counterexample from the article
module Counterexample where

import Contracts
import Prelude (Bool(..),undefined,error)

not True  = False
not False = True

True  && b = b
False && _ = False

-- loop = UNR
loop = loop

-- Dimitrios' original example.
-- Fails indeed, and we see the parallel or function in the model
f :: Bool -> (Bool -> Bool -> Bool) -> Bool
f b h | h True b && h b True && not (h False False)
        = if h True loop && h loop True then error "bad" else True
f b h = True

counter_oops = (f ::: CF :-> \ b -> (CF --> CF --> CF) :-> \ (|||) -> CF)
  `Using` (loop ::: CF)

-- This was the one written in the POPL submission, but it actually holds
f_popl :: (Bool -> Bool -> Bool) -> Bool
f_popl h = if (h True True) && (not (h False False))
            then if (h True loop) && (h loop True)
                    then undefined else True
            else True

counter_popl = f_popl ::: (CF --> CF --> CF) --> CF
  `Using` (loop ::: CF)
