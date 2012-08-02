{-

    These are tricky to translate because they will case on the same
    variable twice, with a default pattern, essentially making a clause
    that looks like this:

        x /= UNR & x /= BAD & x /= True & x /= False -> f(x) = BAD,

    but this case does not really exist.

    These examples fail when optimisation is turned off.

-}
module DefaultBehaviour where

import Contracts
import Prelude(Bool(..),error)

data Ok = Ok

mini :: Bool -> Bool -> Ok
mini True  y    = Ok
mini x     True = Ok
mini False y    = Ok

mini_cf = mini ::: CF --> CF --> CF

-- As or
(||) :: Bool -> Bool -> Bool
True  || y    = True
x     || True = True
False || y    = False

or_cf = (||) ::: CF --> CF --> CF

-- Another example, that didn't work even with optimisation

data T = A | B | C | D

f :: T -> Bool
f A = True
f B = True
f x = g x

{-# NOINLINE g #-}
g :: T -> Bool
g C = True
g D = True
g x = error "blaaa"
    -- ^ this case never happens, but how defaults are translated
    --   we get untyped things to go to BAD rather than UNR

f_cf = f ::: CF --> CF

-- Same as above but with some arguments

data T' = A' Bool | B' Bool | C' T | D' T

f' :: T' -> Bool
f' (A' _) = True
f' (B' _) = True
f' x = g' x

{-# NOINLINE g' #-}
g' :: T' -> Bool
g' (C' _)= True
g' (D' _) = True
g' x = error "blaaa"
    -- ^ this case never happens, but how defaults are translated
    --   we get untyped things to go to BAD rather than UNR

f'_cf = f' ::: CF --> CF
