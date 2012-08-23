{-# LANGUAGE RecordWildCards #-}
{-

    The extra theory the contracts checker does on top of HALO,
    i.e. subtheories about CF,UNR and BAD.

-}
module Contracts.Theory where

import TyCon

import Halo.Subtheory
import Halo.Shared
import Halo.FOL.Abstract
import Halo.Binds

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
    show (CrashFree tc) = "CrashFree " ++ showOutputable tc

instance Clausifiable HCCExtras where
    mkClause _ = clause axiom

type HCCContent   = Content HCCExtras

type HCCSubtheory = Subtheory HCCExtras

type HCCBinds     = BindMap HCCExtras

type HCCBindPart  = BindPart HCCExtras

makeDataDepend :: HCCSubtheory -> HCCSubtheory
makeDataDepend s@(Subtheory{..}) = case provides of
    Data ty_con -> s { depends = Specific (CrashFree ty_con) : depends }
    _           -> s
