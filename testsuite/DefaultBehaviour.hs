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
import Prelude(Bool(..))

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
