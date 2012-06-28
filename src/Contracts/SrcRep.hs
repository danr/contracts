module Contracts.SrcRep where

import Var
import Outputable

import Data.List

contrStrWith :: String -> String -> Bool
contrStrWith end str = "Contracts." `isPrefixOf` str && end `isSuffixOf` str

isStatementType :: Var -> Bool
isStatementType = contrStrWith "Statement" . showSDoc . ppr . varType

isStatementUsing :: Var -> Bool
isStatementUsing = contrStrWith "Using" . show

isStatementCon  :: Var -> Bool
isStatementCon  = contrStrWith ":::" . show

isContrPi       :: Var -> Bool
isContrPi       = contrStrWith ":->" . show

isContrArr      :: Var -> Bool
isContrArr      = contrStrWith "-->" . show

isContrCF       :: Var -> Bool
isContrCF       = contrStrWith "CF" . show

isContrAnd      :: Var -> Bool
isContrAnd      = contrStrWith ":&:" . show

isContrPred     :: Var -> Bool
isContrPred     = contrStrWith "Pred" . show
