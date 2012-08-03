{-# LANGUAGE DeriveDataTypeable #-}
module Contracts.Params where

import System.Console.CmdArgs

data Params = Params
    { files             :: [FilePath]

    , no_min            :: Bool
    , min_as_not_unr    :: Bool

    , fof               :: Bool
    , comments          :: Bool
    , core_optimise     :: Bool
    , dollar_min        :: Bool
    , or_discr          :: Bool
    , fpi_no_plain      :: Bool
    , fpi_no_base       :: Bool
    , quick_tptp        :: Bool

    , db_float_out      :: Bool
    , db_ty_cons        :: Bool
    , db_classes        :: Bool
    , db_names          :: Bool
    , db_lift           :: Bool
    , db_halo           :: Bool
    , db_collect        :: Bool
    , db_trans          :: Bool
    , db_inliner        :: Bool

    , dump_init_core    :: Bool
    , dump_float_out    :: Bool
    , dump_lifted_core  :: Bool
    , dump_rm_def_core  :: Bool
    , dump_inlined_core :: Bool
    , dump_final_core   :: Bool
    , dump_fpi_core     :: Bool
    , dump_tptp         :: Bool
    , dump_contracts    :: Bool
    , dump_subthys      :: Bool
    }
  deriving (Show,Data,Typeable)

defParams :: Params
defParams = Params
    { files             = []    &= args   &= typFile

    , no_min            = False &= groupname "\nSettings for generated theories"
                                &= name "m" &= help "Remove all occurences of min in generated theories"
    , min_as_not_unr    = False &= name "u" &= help "Replace all occurences of min in generated theories with not unr"

    , fof               = False &= name "f" &= help "Always generate clauses in fof"
    , comments          = False &= name "C" &= help "Print comments in TPTP file"
    , core_optimise     = False &= name "O" &= help "Run the core2core optimising pass"
    , dollar_min        = False &= name "d" &= help "Let the min predicate be called $min, efficient for equinox, unparseable for z3"
    , or_discr          = False &= name "o" &= help "Use Or instead of And in the assumptions of discrimination axioms"
    , fpi_no_base       = False &= name "b" &= help "If fpi is applicable, don't generate the base case"
    , fpi_no_plain      = False &= name "i" &= help "If fpi is applicable, don't generate without induction"
    , quick_tptp        = False &= name "Q" &= help "Enable quicker generation of TPTP with variable names from Uniques. Uses cnf and $min and writes no comments."

    , db_float_out      = False &= groupname "\nDebugging output"
                                &= help "Debug floating out (sets Opt_D_dump_simpl_stats and Opt_D_verbose_core2core)"
    , db_classes        = False &= help "Debug information about found classes found"
    , db_ty_cons        = False &= help "Debug some information about found TyCons"
    , db_names          = False &= help "Debug information about the top level names in Core"
    , db_lift           = False &= help "Debug the let-/case-lifter"
    , db_halo           = False &= help "Debug the Haskell to Logic translator"
    , db_collect        = False &= help "Debug collecting contracts (Core -> internal repr)"
    , db_trans          = False &= help "Debug translating contracts (internal repr -> FOL)"
    , db_inliner        = False &= help "Debug inliner (show inlinings of defininitons expressions)"

    , dump_init_core    = False &= help "Dump initial (possibly optimised) core from GHC"
    , dump_float_out    = False &= help "Dump core after GHC's lambda-lifting"
    , dump_lifted_core  = False &= help "Dump core after Halo's let-/case-/lambda-lifting"
    , dump_rm_def_core  = False &= help "Dump core after removing DEFAULT branches"
    , dump_inlined_core = False &= help "Dump core after inliner"
    , dump_final_core   = False &= help "Dump (final) core without Statements"
    , dump_fpi_core     = False &= help "Dump core after fixpoint generation"
    , dump_tptp         = False &= help "Dump all generated tptp"
    , dump_contracts    = False &= help "Dump the internal representation of contracts"
    , dump_subthys      = False &= help "Dump the subtheories needed for contracts"
    }
    &= summary "Haskell Contracts Checker v0.2 Dan Rosén danr@student.gu.se"
    &= program "hcc"
    &= verbosity
