-- Dimitrios' higher-order examples
module HigherOrder where

import Prelude hiding (id,not,const,(==),all,filter)

import Contracts


id x = x

not True = False
not False = True


const x y = x

data Nat = Z | S Nat


isEven :: Nat -> Bool
isEven Z = True
isEven (S x) = not (isEven x)

even_contr = isEven ::: CF --> CF


data List = Nil | Cons Nat List

isNil :: List -> Bool
isNil Nil = True
isNil (Cons x xs) = False

all :: (Nat -> Bool) -> List -> Bool
all f Nil = True
all f (Cons x xs) = if f x then (all f xs) else False

filter :: (Nat -> Bool) -> List -> List
filter f Nil = Nil
filter f (Cons x xs)
  = if (f x) then x `Cons` filter f xs else filter f xs


-- Should hold
unsat_all_contr = all ::: (CF --> CF) --> CF --> CF

unsat_filter_contr  = filter ::: ((CF --> CF) :-> \f -> CF --> CF :&: Pred (all f))
                     `Using` unsat_all_contr

glop = filter isEven  (Cons (S Z) (Cons (S (S (S Z))) Nil))

unsat_glop_contr = glop ::: CF :&: Pred isNil

-- An all with a bug
all_buggy :: (Nat -> Bool) -> List -> Bool
all_buggy f Nil = False -- There is a bug!
all_buggy f (Cons x xs) = if f x then (all_buggy f xs) else False

unsat_all_buggy_contr = all_buggy ::: (CF --> CF) --> CF --> CF

sat_filter_contr  = filter ::: ((CF --> CF) :-> \f -> CF --> CF :&: Pred (all_buggy f))
                    `Using` unsat_all_buggy_contr




