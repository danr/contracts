module MiniUseModule where

import Contracts
import MiniModule
import Prelude ()

plus_cf = (+) ::: CF --> CF --> CF
