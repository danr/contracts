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
withMany withFoo (x:xs) f = withFoo x (\x' ->
                              withMany withFoo xs (\xs' -> f (x':xs')))

-- This function is interesting because it has higher order depth 2

big_unsat_wmc :: Statement
big_unsat_wmc = withMany ::: (CF --> (CF --> CF) --> CF)
                         --> CF --> (CF --> CF) --> CF
