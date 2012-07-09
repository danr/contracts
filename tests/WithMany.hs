module WithMany where

import Contracts

-- marshalling lists of storable objects
-- -------------------------------------

-- |Replicates a @withXXX@ combinator over a list of objects, yielding a list of
-- marshalled objects
--
withMany :: (a -> (b -> res) -> res)  -- withXXX combinator for one object
         -> [a]                       -- storable objects
         -> ([b] -> res)              -- action on list of marshalled obj.s
         -> res
withMany _       []     f = f []
withMany withFoo (x:xs) f = withFoo x (\x2 ->
                              withMany withFoo xs (\xs2 -> f (x2:xs2)))

-- This function is interesting because it has higher order depth 2

unsat_wmc :: Statement
unsat_wmc = withMany ::: (CF --> (CF --> CF) --> CF)
                --> CF --> (CF --> CF) --> CF
