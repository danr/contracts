{-# LANGUAGE PatternGuards #-}
{-

    The source representation of contracts.

-}
module Contracts.SrcRep where

import Var
import Outputable
import Type
import Name
import Module

import Data.List

-- | Is this name defined in the `Contracts' module?
contractModuleName :: Name -> Bool
contractModuleName
    = maybe False (== "Contracts")
    . fmap (moduleNameString . moduleName)
    . nameModule_maybe

contrStrWith :: String -> String -> Bool
contrStrWith end str = "Contracts." `isPrefixOf` str && end `isSuffixOf` str

statementType :: Type -> Bool
statementType ty
    | Just (ty_con,[_ty]) <- splitTyConApp_maybe ty
        = contrStrWith "Statement" . showSDoc . ppr $ ty_con
    | otherwise = False

isStatementType :: Var -> Bool
isStatementType v =
    let (args,res) = splitFunTys . dropForAlls . varType $ v
    in  statementType res && null args

isStatementAssuming :: Var -> Bool
isStatementAssuming = contrStrWith ":=>" . show

isStatementAll :: Var -> Bool
isStatementAll = contrStrWith "All" . show

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
