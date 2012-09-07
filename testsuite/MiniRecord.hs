module MiniRecord where

import Contracts
import Prelude(Bool(..))

data Record
    = Apa
        { first  :: ()
        , second :: ()
        , fourth :: ()
        }
    | Bepa
        { first  :: ()
        , third  :: ()
        , fourth :: ()
        }
    | Cepa
        { second :: ()
        , third  :: ()
        , fourth :: ()
        }

first_cf_oops  = first  ::: CF --> CF
second_cf_oops = second ::: CF --> CF
third_cf_oops  = third  ::: CF --> CF
fourth_cf      = fourth ::: CF --> CF
