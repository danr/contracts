module Test where

import Nat
import Contracts
import Prelude (id,Bool(..))

-- A small unit test, four is recursive_true (and crash-free!)
-- id_z_rec_true   = id Z     ::: CF :&: Pred recursive_true
-- id_one_rec_true = id (S Z) ::: CF :&: Pred recursive_true

recursive_true :: Nat -> Bool
recursive_true Z     = True
recursive_true (S x) = recursive_true x

zero = Z
zero_rec_true = zero ::: CF :&: Pred recursive_true

one = S Z
one_rec_true = one ::: CF :&: Pred recursive_true

two = S (S Z)
two_rec_true = two ::: CF :&: Pred recursive_true

two_cf = two ::: CF

three = S (S (S Z))
three_rec_true = three ::: CF :&: Pred recursive_true

id_four = id (S (S (S (S Z))))
id_four_rec_true = id_four ::: CF :&: Pred recursive_true

id_zero = id Z
id_zero_rec_true = id_zero ::: CF :&: Pred recursive_true

id_one = id (S Z)
id_one_rec_true = id_one ::: CF :&: Pred recursive_true

id_two = id (S (S Z))
id_two_rec_true = id_two ::: CF :&: Pred recursive_true

id_three = id (S (S (S Z)))
id_three_rec_true = id_three ::: CF :&: Pred recursive_true
