module Contracts.SrcRep where

import Var
import Outputable
import Type

import Data.List

-- isContractTyCon :: TyCon -> Bool
-- isContractTyCon =

contrStrWith :: String -> String -> Bool
contrStrWith end str = "Contracts." `isPrefixOf` str && end `isSuffixOf` str

statementType :: Type -> Bool
statementType = contrStrWith "Statement" . showSDoc . ppr

isStatementType :: Var -> Bool
isStatementType v =
    let (args,res) = splitFunTys . dropForAlls . varType $ v
    in  statementType res && not (any statementType args)

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
