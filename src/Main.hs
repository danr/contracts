{-# LANGUAGE RecordWildCards,ViewPatterns #-}
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

import Contracts.Make
import Contracts.Trans
import Contracts.Types
import Contracts.Params
import Contracts.FixpointInduction

import Control.Monad

import System.Environment
import System.Exit
import System.FilePath

import System.Console.CmdArgs

main :: IO ()
main = do

    params@Params{..} <- cmdArgs defParams

    when (null files) $ do
        putStrLn "No input files!"
        putStrLn ""

    forM_ files $ \(dropExtension -> file) -> do

        let dsconf = DesugarConf
                         { debug_float_out = db_float_out
                         , core2core_pass  = core_optimise
                         }

        (modguts,dflags) <- desugar dsconf file

        let core_binds = mg_binds modguts

        us <- mkSplitUniqSupply 'c'

        let ((r,msgs_collect_contr),us1) = collectContracts us core_binds

        (program,stmts) <- case r of
            Right (stmts,bs) -> do
                when dump_contracts (mapM_ print stmts)
                return (bs,stmts)
            Left err -> do
                putStrLn err
                exitFailure

        floated_prog <- lambdaLift dflags program

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

            halt_conf :: HaloConf
            halt_conf = sanitizeConf $ HaloConf
                { use_min      = not no_min
                , use_cf       = True
                , unr_and_bad  = True
                , ext_eq       = False
                -- ^ False for now, no good story about min and ext-eq
                }

            ((lifted_prog,msgs_lift),us2)
                = caseLetLift floated_prog us1

            ((fix_prog,fix_info),us3)
                = initUs us2 (fixpointCoreProgram lifted_prog)

            halt_env_without_hyp_arities
                = mkEnv halt_conf ty_cons_with_builtin fix_prog

            halt_env = halt_env_without_hyp_arities
                { arities = fpiFixHypArityMap fix_info
                                (arities halt_env_without_hyp_arities)
                }

            (subtheories,msgs_trans)
                = translate halt_env ty_cons_with_builtin fix_prog

            printMsgs msgs = unless (null msgs) $ putStrLn $ unlines msgs

            endl = putStrLn "\n"

            printCore msg core = do
                putStrLn $ msg ++ ":\n"
                mapM_ (printDump . ppr) core
                endl

        when dump_init_core (printCore "Original core" core_binds)
        when dump_float_out (printCore "Lambda lifted core" floated_prog)
        when db_lift        (printMsgs msgs_lift)
        when dump_core      (printCore "Final, case/let lifted core" lifted_prog)
        when dump_fpi_core  (printCore "Fixpoint induction core" fix_prog)
        when db_halo        (printMsgs msgs_trans)

        when dump_tptp $ do
            let tptp = linTPTP (strStyle (not no_comments) cnf)
                                ( renameClauses
                                . map minAsNotUnr
                                . (no_min ? removeMins)
                                . concatMap toClauses
                                $ subtheories )
            putStrLn tptp

        when db_make_contracts (printMsgs msgs_collect_contr)

        forM_ stmts $ \stmt@(Statement{..}) -> do
            let (proofs,msgs_tr_contr)
                    = runHaloM halt_env (trStatement fix_info stmt)

            when db_trans_contracts (printMsgs msgs_tr_contr)

            forM_ proofs $ \(proof_part,(clauses,deps)) -> do

                let subtheories' = trim (PrimConAxioms:Data boolTyCon:deps) subtheories
                    tptp = linTPTP (strStyle (not no_comments) cnf)
                                   ( renameClauses
                                   . map minAsNotUnr
                                   . (no_min ? removeMins)
                                   . (++ clauses)
                                   . concatMap toClauses
                                   $ subtheories')

                let filename = show statement_name ++
                               proofPartSuffix proof_part ++ ".tptp"

                putStrLn $ "Writing " ++ show filename

                writeFile filename tptp

