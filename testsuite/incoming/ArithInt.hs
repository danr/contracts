module ArithInt where

import Contracts
import Prelude (Bool(..))
import GHC.Base hiding (Ord(..),(==))

(+)  = plusInt
(*)  = timesInt
(-)  = minusInt
(==) = eqInt
(/=) = neInt
(<)  = ltInt
(<=) = leInt
(>=) = geInt
(>)  = gtInt

plus :: Int -> Int -> Int
plus = (+)

plus_cf = plus ::: CF --> CF --> CF

natural :: Int -> Bool
natural x = x >= 0

cf_nat = CF :&: Pred natural

plus_nat_nat_nat = plus ::: cf_nat --> cf_nat --> cf_nat

sum3 :: Int -> Int -> Int -> Int
sum3 x y z = x + y + (z + z)

sum3_cf = sum3 ::: CF --> CF --> CF --> CF

addOne :: Int -> Int
addOne = (+ 1)

addOne_cf = addOne ::: CF --> CF

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial_cf = factorial ::: CF --> CF

factorial' :: Int -> Int
factorial' n
    | n < 0     = error "Factorial on negative number!"
    | n == 0    = 1
    | otherwise = n * factorial' (n - 1)

factorial'_cf     = factorial' ::: CF --> CF
factorial'_nat_cf = factorial' ::: cf_nat --> CF

ilya = 1 + 1 ::: Pred (== 2)
