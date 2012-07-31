module Record where

import Contracts
import Prelude(Bool(..))

data Record
    = Apa
        { first  :: Bool
        , second :: [Bool]
        , fourth :: ()
        }
    | Bepa
        { first  :: Bool
        , third  :: (Bool,())
        , fourth :: ()
        }
    | Cepa
        { second :: [Bool]
        , third  :: (Bool,())
        , fourth :: ()
        }

first_cf_oops  = first  ::: CF --> CF
second_cf_oops = second ::: CF --> CF
third_cf_oops  = third  ::: CF --> CF
fourth_cf      = fourth ::: CF --> CF
