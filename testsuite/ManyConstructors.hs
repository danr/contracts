module ManyConstructors where

import Contracts
import Prelude(Bool(..))
import Properties

data K = A | B | C | D | E | F

(==) :: K -> K -> Bool
A == A = True
B == B = True
C == C = True
D == D = True
E == E = True
F == F = True
_ == _ = False

eq_cf = (==) ::: CF --> CF --> CF

data K2
    = A1 | B1 | C1 | D1 | E1 | F1
    | A2 | B2 | C2 | D2 | E2 | F2

(===) :: K2 -> K2 -> Bool
A1 === A1 = True
B1 === B1 = True
C1 === C1 = True
D1 === D1 = True
E1 === E1 = True
F1 === F1 = True
A2 === A2 = True
B2 === B2 = True
C2 === C2 = True
D2 === D2 = True
E2 === E2 = True
F2 === F2 = True
_  === _  = False

eq_2_cf = (===) ::: CF --> CF --> CF

data K4
    = Aa | Ba | Ca | Da | Ea | Fa
    | Ab | Bb | Cb | Db | Eb | Fb
    | Ac | Bc | Cc | Dc | Ec | Fc
    | Ad | Bd | Cd | Dd | Ed | Fd

(====) :: K4 -> K4 -> Bool
Aa ==== Aa = True
Ba ==== Ba = True
Ca ==== Ca = True
Da ==== Da = True
Ea ==== Ea = True
Fa ==== Fa = True
Ab ==== Ab = True
Bb ==== Bb = True
Cb ==== Cb = True
Db ==== Db = True
Eb ==== Eb = True
Fb ==== Fb = True
Ac ==== Ac = True
Bc ==== Bc = True
Cc ==== Cc = True
Dc ==== Dc = True
Ec ==== Ec = True
Fc ==== Fc = True
Ad ==== Ad = True
Bd ==== Bd = True
Cd ==== Cd = True
Dd ==== Dd = True
Ed ==== Ed = True
Fd ==== Fd = True
_  ==== _  = False

eq_4_cf = (====) ::: CF --> CF --> CF

-- Reflexivity, symmetry and transitivity

-- Transitivity does not hold since
--    A === UNR is True or UNR
--    UNR === B is True or UNR
--    But A === B is not True and not UNR.

eq_refl           = reflexive (==)
eq_sym            = symmetric (==)
eq_trans_broken   = transitive (==)
eq_trans_broken_  = transitive (==) `Using` eq_refl
                                    `Using` eq_sym
                                    `Using` eq_cf

eq_2_refl         = reflexive (===)
eq_2_sym          = symmetric (===)
eq_2_trans_broken = transitive (===)

eq_4_refl         = reflexive (====)
eq_4_sym          = symmetric (====)
eq_4_trans_broken = transitive (====)

