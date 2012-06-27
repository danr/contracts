module Good where

import Contracts
import Prelude hiding (id,not)

my_true  = True
my_false = False

id x = x

not True = False
not False = True

-- Should succeed

true_good      = my_true  ::: CF

true_holds     = my_true  ::: Pred id

true_holds_cf  = my_true  ::: CF :&: Pred id

false_not      = my_false ::: Pred not

false_not_cf   = my_false ::: CF :&: Pred not

id_cf_to_cf    = id       ::: CF --> CF

id_id          = id       ::: CF --> Pred id

id_id_and_cf   = id       ::: CF --> CF :&: Pred id

not_cf_to_cf   = not      ::: CF --> CF

not_not        = not      ::: CF --> Pred not

not_not_and_cf = not      ::: CF --> CF :&: Pred not

-- Should fail --

true_not       = my_true  ::: Pred not

false_holds    = my_false ::: Pred id

true_not_cf    = my_true  ::: CF :&: Pred not

false_holds_cf = my_false ::: CF :&: Pred id

not_id         = not      ::: CF --> Pred id

not_id_cf      = not      ::: CF --> CF :&: Pred id

id_not         = id       ::: CF --> Pred not

id_not_cf      = id       ::: CF --> CF :&: Pred not
