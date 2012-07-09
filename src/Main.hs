{-# LANGUAGE RecordWildCards,ViewPatterns,DisambiguateRecordFields #-}
module Main where

import BasicTypes
import GHC
import HscTypes
import Outputable
import TysWiredIn
import UniqSupply

import Halo.Conf
import Halo.Entry
import Halo.FOL.Linearise
import Halo.FOL.MinAsNotUnr
import Halo.FOL.RemoveMin
import Halo.FOL.Rename
import Halo.FOL.Style
import Halo.Lift
import Halo.Monad
import Halo.Subtheory
import Halo.Trans
import Halo.Trim
import Halo.Util ((?))

import Contracts.Collect
import Contracts.Trans
import Contracts.Types
import Contracts.Params as Params
import Contracts.FixpointInduction
import Contracts.Theory
import Contracts.Axioms

import Control.Monad

import System.Environment
import System.Exit
import System.FilePath

import System.Console.CmdArgs

printMsgs msgs = unless (null msgs) $ putStrLn $ unlines msgs

endl = putStrLn "\n"

printCore msg core = do
    putStrLn $ msg ++ ":\n"
    mapM_ (printDump . ppr) core
    endl

processFile :: Params -> FilePath -> IO ()
processFile params@Params{..} file = do

    let dsconf = DesugarConf
                     { debug_float_out = db_float_out
                     , core2core_pass  = core_optimise
                     }


    (modguts,dflags) <- desugar dsconf file

    let core_binds = mg_binds modguts

    when dump_init_core (printCore "Original core" core_binds)

    us <- mkSplitUniqSupply 'c'

    let (collect_either_res,us2) = initUs us (collectContracts core_binds)

    (stmts,program,msgs_collect_contr) <- case collect_either_res of
        Right res@(stmts,_,_) -> do
            when dump_contracts (mapM_ print stmts)
            return res
        Left err -> do
            putStrLn err
            exitFailure

    floated_prog <- lambdaLift dflags program

    when db_collect (printMsgs msgs_collect_contr)

    when dump_float_out (printCore "Lambda lifted core" floated_prog)

    let ((lifted_prog,msgs_lift),us3) = caseLetLift floated_prog us2

    when db_lift    (printMsgs msgs_lift)
    when dump_core  (printCore "Final, case/let lifted core" lifted_prog)

    let ty_cons :: [TyCon]
        ty_cons = mg_tcs modguts

        ty_cons_with_builtin :: [TyCon]
        ty_cons_with_builtin
            = listTyCon : boolTyCon : unitTyCon
            : [ tupleTyCon BoxedTuple size
              | size <- [0..8]
              -- ^ choice: only tuples of size 0 to 8 supported!
              ]
            ++ ty_cons

        halo_conf :: HaloConf
        halo_conf = sanitizeConf $ HaloConf
            { use_min           = not no_min
            , use_minrec        = False
            , unr_and_bad       = True
            , ext_eq            = False
            -- ^ False for now, no good story about min and ext-eq
            , disjoint_booleans = True -- not squishy_booleans
            , or_discr          = or_discr
            }

        ((fix_prog,fix_info),us4)
            = initUs us3 (fixpointCoreProgram lifted_prog)

        halt_env_without_hyp_arities
            = mkEnv halo_conf ty_cons_with_builtin fix_prog

        halt_env = halt_env_without_hyp_arities
            { arities = fpiFixHypArityMap fix_info
                            (arities halt_env_without_hyp_arities)
            }

        (subtheories_unfiddled,msgs_trans)
            = translate halt_env ty_cons_with_builtin fix_prog

        subtheories
            = primConAxioms
            : primConApps
            : mkCF ty_cons_with_builtin ++
            (map makeDataDepend subtheories_unfiddled)

    when dump_fpi_core (printCore "Fixpoint induction core" fix_prog)
    when db_halo       (printMsgs msgs_trans)

    let toTPTP extra_clauses
            = linTPTP (strStyle (StyleConf { style_comments   = comments
                                           , style_cnf        = not fof
                                           , style_dollar_min = dollar_min
                                           }))
            . renameClauses
            . (min_as_not_unr ? map minAsNotUnr)
            . (no_min ? removeMins)
            . (++ extra_clauses)
            . concatMap toClauses

    when dump_tptp $ putStrLn (toTPTP [] subtheories)

    forM_ stmts $ \stmt@Statement{..} -> do
        let (proofs,msgs_tr_contr)
                = runHaloM halt_env (trStatement params stmts fix_info stmt)

        when db_trans (printMsgs msgs_tr_contr)

        forM_ proofs $ \(proof_part,(clauses,deps)) -> do

            let important    = Specific PrimConAxioms:Data boolTyCon:deps
                subtheories' = trim important subtheories

                tptp = toTPTP clauses subtheories'

                filename = show statement_name ++
                               proofPartSuffix proof_part ++ ".tptp"

            putStrLn $ "Writing " ++ show filename

            when dump_subthys $ do
                putStrLn $ "Subtheories: "
                mapM_ print subtheories'

            writeFile filename tptp

main :: IO ()
main = do

    params@Params{..} <- cmdArgs defParams

    when (null files) $ do
        putStrLn "No input files!"
        putStrLn ""

    mapM_ (processFile params . dropExtension) files
