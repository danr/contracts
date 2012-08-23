module SimpleExtensions where

import Contracts
import Prelude hiding (id,not,const,(==),flip)

id x = x

not True = False
not False = True

const x y = x

flip f x y = f y x

bad :: Bool
bad = error "bad!"

boom :: a -> Bool
boom = error "boom!"

True  == True  = True
False == False = True
_     == _     = False

-- Should succeed --

false_assumption1_      = bad   ::: CF       :=> bad   ::: CF
false_assumption2_      = False ::: Pred id  :=> False ::: Pred id

flip_const_bad_cf      = flip const bad ::: CF --> CF

param_cf_const         = All (\x -> x ::: CF :=> const x ::: CF --> CF)

param_const_boom       = All (\x -> (x ::: CF) :=> const x ::: Pred boom --> CF)
param_const_cf         = All (\x -> (x ::: CF) :=> const x ::: CF --> CF)
param_const_cf_eq      = All (\x -> (x ::: CF) :=> const x ::: CF --> CF :&: Pred (x ==))
param_id_cf            = All (\x -> (x ::: CF) :=> id x    ::: CF)
param_id_eq            = All (\x -> (x ::: CF) :=> id x    ::: Pred (x ==))
param_not_cf           = All (\x -> (x ::: CF) :=> not x   ::: CF)
param_not_uneq         = All (\x -> (x ::: CF) :=> not x   ::: Pred (\y -> not (x == y)))
param_juggle_id        = All (\x -> (x ::: Pred id ) :=> id x  ::: Pred id)
param_juggle_not       = All (\x -> (x ::: Pred not) :=> not x ::: Pred id)
param_bad_unjuggle_id  = All (\x -> (x ::: Pred not) :=> id x  ::: Pred not)
param_bad_unjuggle_not = All (\x -> (x ::: Pred id ) :=> not x ::: Pred not)
param_juggle_id_cf     = All (\x -> (x ::: CF :&: Pred id ) :=> id x  ::: Pred id)
param_juggle_not_cf    = All (\x -> (x ::: CF :&: Pred not) :=> not x ::: Pred id)
param_unjuggle_cf_id   = All (\x -> (x ::: CF :&: Pred not) :=> id x  ::: CF :&: Pred not)
param_unjuggle_cf_not  = All (\x -> (x ::: CF :&: Pred id ) :=> not x ::: CF :&: Pred not)
param_unjuggle_id      = All (\x -> (x ::: CF :&: Pred not) :=> id x  ::: Pred not)
param_unjuggle_not     = All (\x -> (x ::: CF :&: Pred id ) :=> not x ::: Pred not)

unused_false_assumption1_ = bad   ::: CF       :=> False ::: Pred id
unused_false_assumption2_ = False ::: Pred id  :=> bad   ::: CF
unused_false_assumption3_ = True  ::: Pred not :=> bad   ::: CF

-- Should fail --

sat_true_assumption1_  = True ::: Pred id :=> bad ::: CF
sat_true_assumption2_  = True ::: CF      :=> bad ::: CF
sat_true_assumption3_  = True ::: CF      :=> False ::: Pred id
sat_true_assumption4_  = True ::: Pred id :=> False ::: Pred id

sat_const_bad_cf      = const bad ::: CF --> CF

sat_param_cf          = All (\x -> x ::: CF)

sat_param_id_cf_const = All (\x -> (x ::: CF) :=> id x  ::: CF :&: Pred (const x))
sat_param_id_cf_id    = All (\x -> (x ::: CF) :=> id x  ::: CF :&: Pred id)
sat_param_id_cf_not   = All (\x -> (x ::: CF) :=> id x  ::: CF :&: Pred not)
sat_param_id_const    = All (\x -> (x ::: CF) :=> id x  ::: Pred (const x))
sat_param_id_id       = All (\x -> (x ::: CF) :=> id x  ::: Pred id)
sat_param_id_not      = All (\x -> (x ::: CF) :=> id x  ::: Pred not)
sat_param_not_cf_id   = All (\x -> (x ::: CF) :=> not x ::: CF :&: Pred id)
sat_param_not_cf_not  = All (\x -> (x ::: CF) :=> not x ::: CF :&: Pred not)
sat_param_not_id      = All (\x -> (x ::: CF) :=> not x ::: Pred id)
sat_param_not_not     = All (\x -> (x ::: CF) :=> not x ::: Pred not)

