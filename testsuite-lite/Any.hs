module Any where

import Contracts
import Prelude (Bool(..), not, (||), (&&) )

data Nat = Z | S Nat

recNat Z     = True
recNat (S x) = recNat x

recBool True  = True
recBool False = True

recNatList [] = True
recNatList (n:ns) = recNat n && recNatList ns

recNatListOp [] = True
recNatListOp (n:ns) = recNatListOp ns && recNat n

any p (x:xs) = p x || any p xs
any p []     = False

any_broken = any :::  (Pred recNat --> Pred recBool)
                 --> Pred recNatList
                 --> Pred recBool
