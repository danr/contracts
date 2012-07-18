{-# LANGUAGE RecordWildCards #-}
module Contracts.Theory where

import Halo.Subtheory
import TyCon
import Halo.FOL.Abstract

data HCCExtras
    = PrimConAxioms
    -- ^ Axioms about UNR and BAD
    | PrimConApps
    -- ^ App on UNR and BAD
    | CrashFree TyCon
    -- ^ CF predicates for a data type
  deriving
    (Eq,Ord)

instance Show HCCExtras where
    show PrimConAxioms  = "PrimConAxioms"
    show PrimConApps    = "PrimConApps"
    show (CrashFree tc) = "CrashFree"

instance Clausifiable HCCExtras where
    mkClause _ = clause axiom

type HCCContent   = Content HCCExtras

type HCCSubtheory = Subtheory HCCExtras

makeDataDepend :: HCCSubtheory -> HCCSubtheory
makeDataDepend s@(Subtheory{..}) = case provides of
    Data ty_con -> s { depends = Specific (CrashFree ty_con) : depends }
    _           -> s
