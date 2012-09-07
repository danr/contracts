module SimpleExtensions where

import Contracts
import Prelude (Bool(..),id,not,const,flip,error,($))

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

param_cf_const         = All $ \x -> x ::: CF :=> const x ::: CF --> CF

param_const_boom       = All $ \x -> (x ::: CF) :=> const x ::: Pred boom --> CF
param_const_cf         = All $ \x -> (x ::: CF) :=> const x ::: CF --> CF
param_const_cf_eq      = All $ \x -> (x ::: CF) :=> const x ::: CF --> CF :&: Pred (x ==)
param_id_cf            = All $ \x -> (x ::: CF) :=> id x    ::: CF
param_id_eq            = All $ \x -> (x ::: CF) :=> id x    ::: Pred (x ==)
param_not_cf           = All $ \x -> (x ::: CF) :=> not x   ::: CF
param_not_uneq         = All $ \x -> (x ::: CF) :=> not x   ::: Pred (\y -> not (x == y))
param_juggle_id        = All $ \x -> (x ::: Pred id ) :=> id x  ::: Pred id
param_juggle_not       = All $ \x -> (x ::: Pred not) :=> not x ::: Pred id
param_bad_unjuggle_id  = All $ \x -> (x ::: Pred not) :=> id x  ::: Pred not
param_bad_unjuggle_not = All $ \x -> (x ::: Pred id ) :=> not x ::: Pred not
param_juggle_id_cf     = All $ \x -> (x ::: CF :&: Pred id ) :=> id x  ::: Pred id
param_juggle_not_cf    = All $ \x -> (x ::: CF :&: Pred not) :=> not x ::: Pred id
param_unjuggle_cf_id   = All $ \x -> (x ::: CF :&: Pred not) :=> id x  ::: CF :&: Pred not
param_unjuggle_cf_not  = All $ \x -> (x ::: CF :&: Pred id ) :=> not x ::: CF :&: Pred not
param_unjuggle_id      = All $ \x -> (x ::: CF :&: Pred not) :=> id x  ::: Pred not
param_unjuggle_not     = All $ \x -> (x ::: CF :&: Pred id ) :=> not x ::: Pred not

unused_false_assumption1_ = bad   ::: CF       :=> False ::: Pred id
unused_false_assumption2_ = False ::: Pred id  :=> bad   ::: CF
unused_false_assumption3_ = True  ::: Pred not :=> bad   ::: CF

-- Should fail --

sat_true_assumption1_  = True ::: Pred id :=> bad ::: CF
sat_true_assumption2_  = True ::: CF      :=> bad ::: CF
sat_true_assumption3_  = True ::: CF      :=> False ::: Pred id
sat_true_assumption4_  = True ::: Pred id :=> False ::: Pred id

sat_const_bad_cf      = const bad ::: CF --> CF

sat_param_cf          = All $ \x -> x ::: CF

sat_param_id_cf_const = All $ \x -> (x ::: CF) :=> id x  ::: CF :&: Pred (const x)
sat_param_id_cf_id    = All $ \x -> (x ::: CF) :=> id x  ::: CF :&: Pred id
sat_param_id_cf_not   = All $ \x -> (x ::: CF) :=> id x  ::: CF :&: Pred not
sat_param_id_const    = All $ \x -> (x ::: CF) :=> id x  ::: Pred (const x)
sat_param_id_id       = All $ \x -> (x ::: CF) :=> id x  ::: Pred id
sat_param_id_not      = All $ \x -> (x ::: CF) :=> id x  ::: Pred not
sat_param_not_cf_id   = All $ \x -> (x ::: CF) :=> not x ::: CF :&: Pred id
sat_param_not_cf_not  = All $ \x -> (x ::: CF) :=> not x ::: CF :&: Pred not
sat_param_not_id      = All $ \x -> (x ::: CF) :=> not x ::: Pred id
sat_param_not_not     = All $ \x -> (x ::: CF) :=> not x ::: Pred not

-- Switcharoo

anyC = Pred (const True)

switch True  x y = x
switch False x y = y

switch_true         = switch True                ::: CF --> anyC --> CF
flip_switch_false   = flip (switch False)        ::: CF --> anyC --> CF
lambda_switch_true  = (\x y -> switch True x y)  ::: CF --> anyC --> CF
lambda_switch_false = (\x y -> switch False y x) ::: CF --> anyC --> CF

sat_switch_true         = switch True                ::: anyC --> CF --> CF
sat_flip_switch_false   = flip (switch False)        ::: anyC --> CF --> CF
sat_lambda_switch_true  = (\x y -> switch True x y)  ::: anyC --> CF --> CF
sat_lambda_switch_false = (\x y -> switch False y x) ::: anyC --> CF --> CF

-- Finding the right function to do recusion on

data Nat = S Nat | Z

ind True  Z     = Z
ind True  (S x) = ind True x
ind False y     = Z

ind_true  = ind True  ::: CF --> CF
ind_false = ind False ::: anyC --> CF

sat_ind_true  = ind True ::: anyC --> CF

{-
-- These don't currently work when splitting fpi goals
lambda x = ind True x

ind_lambda_true  = lambda ::: CF --> CF
ind_lambda_true  = (\x -> ind True x) ::: CF --> CF

ind_flip_flip_true  = flip (flip ind) True  ::: CF --> CF
ind_flip_flip_false = flip (flip ind) False ::: anyC --> CF
-}

-- Proving something about everything

everything_anyC     = All (::: anyC)
sat_everything_cf   = All (::: CF)
sat_everything_true = All (::: Pred id)

