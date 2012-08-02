module Major where

import Contracts
import Prelude(Bool(..))

-- From Ulf Norell's thesis
maj :: Bool -> Bool -> Bool -> Bool
maj True  True  True  = True
maj True  False z     = z
maj False y     True  = y
maj x     True  False = x
maj False False False = False

-- The GHC Core of this function is a bit of a mess
-- With optimisation off, it generates annoying DEFAULT cases
maj_cf = maj ::: CF --> CF --> CF --> CF

-- An alternative definition, also from Norell's thesis
maj' _     False False = False
maj' x     True  False = x
maj' False y     True  = y
maj' True  _     True  = True

maj'_cf = maj' ::: CF --> CF --> CF --> CF
