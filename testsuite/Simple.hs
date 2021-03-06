module Simple where

import Contracts
import Prelude hiding (id,not,const,(==))

id x = x

not True = False
not False = True

const x y = x

bad :: Bool
bad = error "bad!"

boom :: a -> Bool
boom = error "boom!"

True  == True  = True
False == False = True
_     == _     = False

-- Should succeed --

const_boom       = const ::: CF --> Pred boom --> CF
const_cf         = const ::: CF --> CF --> CF
const_cf_eq      = const ::: CF :-> \ v -> CF --> CF :&: Pred (v ==)
false_cf         = False ::: CF
false_cf_not     = False ::: CF :&: Pred not
false_not        = False ::: Pred not
id_cf            = id    ::: CF --> CF
id_eq            = id    ::: CF :-> \ v -> Pred (v ==)
juggle_id_cf     = id    ::: CF :&: Pred id  --> Pred id
juggle_not_cf    = not   ::: CF :&: Pred not --> Pred id
not_cf           = not   ::: CF --> CF
not_uneq         = not   ::: CF :-> \ v -> Pred (\x -> not (v == x))
true_cf_id       = True  ::: CF :&: Pred id
true_cf          = True  ::: CF
true_id          = True  ::: Pred id
unjuggle_cf_id   = id    ::: CF :&: Pred not --> CF :&: Pred not
unjuggle_cf_not  = not   ::: CF :&: Pred id  --> CF :&: Pred not
unjuggle_id      = id    ::: CF :&: Pred not --> Pred not
unjuggle_not     = not   ::: CF :&: Pred id  --> Pred not
juggle_id        = id    ::: Pred id  --> Pred id
juggle_not       = not   ::: Pred not --> Pred id
bad_unjuggle_id  = id    ::: Pred not --> Pred not
bad_unjuggle_not = not   ::: Pred id  --> Pred not

-- Should fail --

sat_boom_unboom  = boom  ::: Pred boom --> CF  -- Satisfiable by bottom!
sat_bad_cf       = bad   ::: CF
sat_bad_cf_id    = bad   ::: CF :&: Pred id
sat_bad_cf_not   = bad   ::: CF :&: Pred not
sat_bad_id       = bad   ::: Pred id
sat_bad_not      = bad   ::: Pred not
sat_boom_bad     = bad   ::: Pred boom
sat_boom_boom    = boom  ::: CF --> Pred boom
sat_const_cf_res = const ::: CF :-> \ v -> CF --> CF :&: Pred (const v)
sat_false_cf_id  = False ::: CF :&: Pred id
sat_false_id     = False ::: Pred id
sat_id_cf_const  = id    ::: CF :-> \ y -> CF :&: Pred (const y)
sat_id_cf_id     = id    ::: CF --> CF :&: Pred id
sat_id_cf_not    = id    ::: CF --> CF :&: Pred not
sat_id_const     = id    ::: CF :-> \ y -> Pred (const y)
sat_id_id        = id    ::: CF --> Pred id
sat_id_not       = id    ::: CF --> Pred not
sat_not_cf_id    = not   ::: CF --> CF :&: Pred id
sat_not_cf_not   = not   ::: CF --> CF :&: Pred not
sat_not_id       = not   ::: CF --> Pred id
sat_not_not      = not   ::: CF --> Pred not
sat_true_cf_not  = True  ::: CF :&: Pred not
sat_true_not     = True  ::: Pred not
