module Bool where

import Prelude(Bool(..))
import Contracts

True  && b = b
False && _ = False

False || b = b
True  || _ = True

not True  = False
not False = True

x ==> y = not x || y

contr_and :: Statement
contr_and = (&&) ::: CF --> CF --> CF

contr_or :: Statement
contr_or = (||) ::: CF --> CF --> CF

contr_not :: Statement
contr_not = not ::: CF --> CF

contr_imp :: Statement
contr_imp = implies :: CF --> CF --> CF
