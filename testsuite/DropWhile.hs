module DropWhile where

import Contracts
import Prelude (Bool(..))

-- dropWhile

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p []     = []
dropWhile p (x:xs) = if p x then dropWhile p xs else x:xs

-- A filter-like contract for dropWhile:
-- The counterexample here is really dull:
--
--     p ?1           = True
--     xs             = ?1 : xs
--     dropWhile p xs = xs
dropWhile_oops = dropWhile
    ::: ((CF --> CF) :-> \p -> CF :-> \xs -> CF :&: Pred (\ys -> not (any p ys)))
    `Using` any_cf


-- Same counterexample
dropWhile_oops_non_rec = dropWhile
    ::: ((CF --> CF) :-> \p -> CF :-> \xs -> CF :&: Pred (\ys -> not (any p ys)))
    `Using` (dropWhile ::: (CF --> CF) --> CF --> CF)
    `Using` any_cf


-- This one holds
dropWhile_suffix = dropWhile
    ::: (CF --> CF) :-> \p -> CF :-> \xs -> CF :&: Pred (suffixNot p)


suffixNot p []     = True
suffixNot p (x:xs) = not (p x)

any :: (a -> Bool) -> [a] -> Bool
any p (x:xs) = p x || any p xs
any p []     = False

any_cf = any ::: (CF --> CF) --> CF --> CF

(||) :: Bool -> Bool -> Bool
False || b = b
True  || _ = True

not :: Bool -> Bool
not True  = False
not False = True