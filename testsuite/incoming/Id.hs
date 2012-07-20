module Id where

import Prelude ()
import Contracts

id :: a -> a
id x = x

id_contr :: Statement
id_contr = id ::: CF :-> \_ -> CF

id_contr' :: Statement
id_contr' = id ::: CF --> CF