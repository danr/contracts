module Counter where

import Contracts
import Prelude (Bool(..),otherwise)

data Nat = S !Nat | Z

True  && x = x
_ && _ = False

Z     <= _     = True
_     <= Z     = False
(S x) <= (S y) = x <= y

id x = x

two = S (S Z)

finite Z     = True
finite (S x) = finite x

small_nats = id ::: CF --> Pred (\x -> two <= x)

