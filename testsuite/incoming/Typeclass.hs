module Typeclass where

import Contracts
import Prelude(Bool(..))

class Id a where
  id :: a -> a

instance Id Bool where
  id x = x

contr_id_bool :: Statement
contr_id_bool = (id :: Bool -> Bool) ::: CF --> CF
