{-# LANGUAGE PatternGuards #-}
{-

    The source representation of contracts.

-}
module Contracts.SourceRepr where

import Var
import Type
import Name
import Module

import Data.List

import Halo.Shared

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
        = contrStrWith "Statement" . showOutputable $ ty_con
    | otherwise = False

isStatementType :: Var -> Bool
isStatementType v =
    let (args,res) = splitFunTys . dropForAlls . varType $ v
    in  statementType res && null args

isStatementAssuming :: Var -> Bool
isStatementAssuming = contrStrWith ":=>" . showOutputable

isStatementAll :: Var -> Bool
isStatementAll = contrStrWith "All" . showOutputable

isStatementUsing :: Var -> Bool
isStatementUsing = contrStrWith "Using" . showOutputable

isStatementCon  :: Var -> Bool
isStatementCon  = contrStrWith ":::" . showOutputable

isContrPi       :: Var -> Bool
isContrPi       = contrStrWith ":->" . showOutputable

isContrArr      :: Var -> Bool
isContrArr      = contrStrWith "-->" . showOutputable

isContrCF       :: Var -> Bool
isContrCF       = contrStrWith "CF" . showOutputable

isContrAnd      :: Var -> Bool
isContrAnd      = contrStrWith ":&:" . showOutputable

isContrPred     :: Var -> Bool
isContrPred     = contrStrWith "Pred" . showOutputable
