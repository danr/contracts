{-# LANGUAGE DeriveDataTypeable #-}
module Contracts.Params where

import System.Console.CmdArgs

data Params = Params
    { files            :: [FilePath]

    , no_min           :: Bool
    , min_as_not_unr   :: Bool

    , fof              :: Bool
    , comments         :: Bool
    , core_optimise    :: Bool
    , dollar_min       :: Bool
    , or_discr         :: Bool
    , fpi_no_plain     :: Bool
    , symmetric_min    :: Bool

    , db_float_out     :: Bool
    , db_lift          :: Bool
    , db_halo          :: Bool
    , db_collect       :: Bool
    , db_trans         :: Bool

    , dump_init_core   :: Bool
    , dump_float_out   :: Bool
    , dump_core        :: Bool
    , dump_fpi_core    :: Bool
    , dump_tptp        :: Bool
    , dump_contracts   :: Bool
    }
  deriving (Show,Data,Typeable)

defParams :: Params
defParams = Params
    { files            = []    &= args   &= typFile

    , no_min           = False &= groupname "\nSettings for generated theories"
                               &= name "m" &= help "Remove all occurences of min in generated theories (default off)"
    , min_as_not_unr   = False &= name "u" &= help "Replace all occurences of min in generated theories with not.unr (default off)"

    , fof              = False &= name "f" &= help "Generate theories in CNF rather than fof when possible"
    , comments         = False &= name "C" &= help "Don't print comments in TPTP file (current default is on for debugging purposes)"
    , core_optimise    = False &= name "O" &= help "Run the core2core optimising pass"
    , dollar_min       = False &= name "d" &= help "Let the min predicate be called $min, efficient for equinox, unparseable for z3"
    , or_discr         = False &= name "o" &= help "Use Or instead of And in the assumptions of discrimination axioms"
    , fpi_no_plain     = False &= name "i" &= help "If you can apply fpi, don't generate without induction"
    , symmetric_min    = False &= name "s" &= help "Use Dimitrios' new symmetric min ideas"

    , db_float_out     = False &= groupname "\nDebugging output"
                               &= help "Debug floating out (sets Opt_D_dump_simpl_stats and Opt_D_verbose_core2core)"

    , db_lift          = False &= help "Debug the let-/case-lifter"
    , db_halo          = False &= help "Debug the Haskell to Logic translator"
    , db_collect       = False &= help "Debug collecting contracts (Core -> internal repr)"
    , db_trans         = False &= help "Debug translating contracts (internal repr -> FOL)"

    , dump_init_core   = False &= help "Dump initial core we get from GHC"
    , dump_float_out   = False &= help "Dump core after lambda-lifting"
    , dump_core        = False &= help "Dump final core after let-/case-lifting"
    , dump_fpi_core    = False &= help "Dump core after fixpoint generation"
    , dump_tptp        = False &= help "Dump all generated tptp"
    , dump_contracts   = False &= help "Dump the internal representation of contracts"
    }
    &= summary "Haskell Contracts Checker v0.1 Dan Rosén danr@student.gu.se"
    &= program "hcc"
