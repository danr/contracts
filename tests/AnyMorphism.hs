
-- any p (xs ++ ys) = any p xs || any p ys

module AnyMorphism where

import Prelude (Bool(..),otherwise)
import Contracts

(<=>) :: Bool -> Bool -> Bool
True  <=> True  = True
False <=> False = True
True  <=> False = False
False <=> True  = False

(||) :: Bool -> Bool -> Bool
True  || _ = True
False || b = b

any :: (a -> Bool) -> [a] -> Bool
any p []     = False
any p (x:xs) = p x || any p xs

unsat_any_cf = any ::: (CF --> CF) --> CF --> CF

-- Stupid append with a skolem predicate (!)
app :: (a -> Bool) -> [a] -> [a] -> [a]
app p (x:xs) ys = x : app p xs ys
app p []     ys = ys

unsat_app_cf = app ::: (CF --> CF) --> CF --> CF --> CF

-- This contract should really be quantified over a p
unsat_app_any_homomorphism =
    app ::: ((CF --> CF) :-> \p
         -> CF :-> \xs
         -> CF :-> \ys
         -> CF :&: Pred (\zs -> any p zs <=> (any p xs || any p ys)))
  `Using` unsat_any_cf

-- This contract should really be quantified over a p
sat_app_any_homomorphism_missing_using =
    app ::: ((CF --> CF) :-> \p
         -> CF :-> \xs
         -> CF :-> \ys
         -> CF :&: Pred (\zs -> any p zs <=> (any p xs || any p ys)))

