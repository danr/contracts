module Test where

import Contracts

data MyNiceType = ConA | ConB | ConC

myTopLevelDefn = ConC

myContract = myTopLevelDefn ::: CF

constantA x = ConA

f $$ x = f x

alsoConstantA = (constantA $$)

-- Shouldn't this be provable?
-- What's missing? Extensional equality?
contractConstantlyA = alsoConstantA ::: CF --> CF
