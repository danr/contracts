
-- any p (xs ++ ys) <=> any p xs || any p ys

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

any_cf = any ::: (CF --> CF) --> CF --> CF

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

given :: Statement -> Statement -> Statement
given x y = Using y x

infixr 0 $

f $ x = f x

-- Now quantified over p
app_any_morphism p =

    given any_cf $
    given (p ::: CF --> CF) $

    (++) ::: CF :-> \xs -> CF :-> \ys ->
             CF :&: Pred (\zs -> any p zs <=> (any p xs || any p ys))
