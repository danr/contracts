{-# LANGUAGE DeriveDataTypeable #-}
module Contracts.Params where

import System.Console.CmdArgs

data Params = Params
    { files             :: [FilePath]

    , paradox_file      :: Maybe FilePath
    , paradox_timeout   :: Int
    , print_raw_model   :: Bool
    , typed_metas       :: Bool

    , no_min            :: Bool
    , min_as_not_unr    :: Bool

    , fof               :: Bool
    , comments          :: Bool
    , core_optimise     :: Bool
    , dollar_min        :: Bool
    , fpi_split         :: Bool
    , fpi_no_plain      :: Bool
    , fpi_no_base       :: Bool
    , quick_tptp        :: Bool
    , or_discr          :: Bool
    , min_or_unr        :: Bool

    , no_skolemisation  :: Bool
    , no_pull_quants    :: Bool
    , case_lift_inner   :: Bool
    , var_scrut_constr  :: Bool

    , db_unfoldings     :: Bool
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
    , dump_unfoldings   :: Bool
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

sanitizeParams :: Params -> Params
sanitizeParams p | no_skolemisation p = p { fpi_split = False }
sanitizeParams p = p

defParams :: Params
defParams = Params
    { files             = []      &= args   &= typFile

    , paradox_file      = Nothing &= groupname "\nPrinting models"
                                  &= help "Pretty-print this file's counter satisfiable model (uses paradox)"
    , paradox_timeout   = 20      &= help "Timeout to paradox (default 20s)"
    , print_raw_model   = False   &= help "Print the raw model from paradox, too"
    , typed_metas       = False   &= name "t" &= help "Print type signatures on meta variables"

    , no_min            = False   &= groupname "\nSettings for generated theories"
                                  &= name "m" &= help "Remove all occurences of min in generated theories"
    , min_as_not_unr    = False   &= name "u" &= help "Replace all occurences of min in generated theories with not unr"
    , or_discr          = False   &= name "o" &= help "Use Or instead of And in the assumptions of discrimination axioms"
    , min_or_unr        = False               &= help "Add the axiom forall x . ~min(x) => cf(x)"

    , fof               = False   &= name "f" &= help "Always generate clauses in fof"
    , comments          = False   &= name "C" &= help "Print comments in TPTP file"
    , core_optimise     = False   &= name "O" &= help "Run the core2core optimising pass"
    , dollar_min        = False   &= name "d" &= help "Let the min predicate be called $min, efficient for equinox, unparseable for z3"
    , fpi_split         = False   &= name "s" &= help "Split into many goals when doing fpi"
    , fpi_no_base       = False   &= name "b" &= help "If fpi is applicable, don't generate the base case"
    , fpi_no_plain      = False   &= name "i" &= help "If fpi is applicable, don't generate without induction"
    , quick_tptp        = False   &= name "Q" &= help "Enable quicker generation of TPTP with variable names from Uniques. Uses cnf and $min and writes no comments."

    , no_skolemisation  = False   &= groupname "\nTesting and comparison"
                                  &= help "Do not skolemise contract"
    , no_pull_quants    = False   &= help "Do not pull quantifiers as far up as possible"
    , case_lift_inner   = False   &= help "Lift all inner cases to top level"
    , var_scrut_constr  = False   &= help "Make a constraint instead of inlining var scrutinees"

    , db_unfoldings     = False   &= groupname "\nDebugging output"
                                  &= help "Debug the unfoldings for found non-local global identifiers"
    , db_float_out      = False   &= help "Debug floating out (sets Opt_D_dump_simpl_stats and Opt_D_verbose_core2core)"
    , db_classes        = False   &= help "Debug information about found classes found"
    , db_ty_cons        = False   &= help "Debug some information about found TyCons"
    , db_names          = False   &= help "Debug information about the top level names in Core"
    , db_lift           = False   &= help "Debug the let-/case-lifter"
    , db_halo           = False   &= help "Debug the Haskell to Logic translator"
    , db_collect        = False   &= help "Debug collecting contracts (Core -> internal repr)"
    , db_trans          = False   &= help "Debug translating contracts (internal repr -> FOL)"
    , db_inliner        = False   &= help "Debug inliner (show inlinings of defininitons expressions)"

    , dump_init_core    = False   &= help "Dump initial (possibly optimised) core from GHC"
    , dump_unfoldings   = False   &= help "Dump unfoldings of non-local Ids"
    , dump_float_out    = False   &= help "Dump core after GHC's lambda-lifting"
    , dump_lifted_core  = False   &= help "Dump core after Halo's let-/case-/lambda-lifting"
    , dump_rm_def_core  = False   &= help "Dump core after removing DEFAULT branches"
    , dump_inlined_core = False   &= help "Dump core after inliner"
    , dump_final_core   = False   &= help "Dump (final) core without Statements"
    , dump_fpi_core     = False   &= help "Dump core after fixpoint generation"
    , dump_tptp         = False   &= help "Dump all generated tptp"
    , dump_contracts    = False   &= help "Dump the internal representation of contracts"
    , dump_subthys      = False   &= help "Dump the subtheories needed for contracts"
    }
    &= summary "Haskell Contracts Checker v0.2 Dan Rosén danr@student.gu.se"
    &= program "hcc"
    &= verbosity
