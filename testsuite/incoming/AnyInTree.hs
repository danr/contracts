module AnyInTree where

import Prelude (Bool(..),otherwise)
import Contracts

data Tree a = Branch (Tree a) a (Tree a) | Empty

(||) :: Bool -> Bool -> Bool
True  || _ = True
False || b = b

inTree :: (a -> Bool) -> Tree a -> Bool
inTree p (Branch l x r) = p x || inTree p l || inTree p r
inTree p Empty          = False

unsat_inTree_cf = inTree ::: (CF --> CF) --> CF --> CF

any :: (a -> Bool) -> [a] -> Bool
any p []     = False
any p (x:xs) = p x || any p xs

unsat_any_cf = any ::: (CF --> CF) --> CF --> CF

(<=>) :: Bool -> Bool -> Bool
True  <=> True  = True
False <=> False = True
True  <=> False = False
False <=> True  = False

-- Stupid append with a skolem predicate (!)
app :: (a -> Bool) -> [a] -> [a] -> [a]
app p (x:xs) ys = x : app p xs ys
app p []     ys = ys

unsat_app_cf = app ::: (CF --> CF) --> CF --> CF --> CF

toList' :: (a -> Bool) -> Tree a -> [a]
toList' p (Branch l x r) = x : app p (toList' p l) (toList' p r)
toList' p Empty          = []

unsat_toList'_cf = toList' ::: (CF --> CF) --> CF --> CF
  `Using` unsat_app_cf

-- This contract should really be quantified over a p
unsat_app_any_homomorphism =
    app ::: ((CF --> CF) :-> \p
         -> CF :-> \xs
         -> CF :-> \ys
         -> CF :&: Pred (\zs -> any p zs <=> (any p xs || any p ys)))
  `Using` unsat_any_cf

-- Cannot prove this yet
-- I get a countermodel, where
--     t = Branch t UNR UNR
--     toList(p,t) = UNR : toList(p,t)
--     p UNR = False
--     any(toList(p,t)) = False
-- BUT
--     inTree(p,t) = True
-- I don't know why yet
unsat_inTree_any =
    inTree
        ::: ( (CF --> CF) --> CF --> CF) :&:
          (  ((CF --> CF) :-> \p
         -> CF :-> \t
         -> CF :&: Pred (\b -> any p (toList' p t) <=> b))
          )
  `Using` unsat_any_cf
  `Using` unsat_toList'_cf
  `Using` unsat_app_cf
  `Using` unsat_app_any_homomorphism

