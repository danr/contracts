{-# LANGUAGE RecordWildCards,ViewPatterns,DisambiguateRecordFields #-}
module Main where

import BasicTypes
import CoreSyn
import GHC
import HscTypes
import Outputable
import TysWiredIn
import UniqSupply
import Var

import Halo.BackgroundTheory
import Halo.Binds
import Halo.Conf
import Halo.Entry
import Halo.FOL.Linearise
import Halo.FOL.Dump
import Halo.FOL.MinAsNotUnr
import Halo.FOL.RemoveMin
import Halo.FOL.Rename
import Halo.Lift
import Halo.Monad
import Halo.Shared
import Halo.Subtheory
import Halo.Trim
import Halo.Util

import Contracts.Axioms
import Contracts.Collect
import Contracts.FixpointInduction
import Contracts.Inliner
import Contracts.Params
import Contracts.Theory
import Contracts.Trans
import Contracts.Types

import Control.Monad
import Control.Monad.Reader

import System.Exit
import System.FilePath

import System.Console.CmdArgs hiding (program)

printMsgs :: [String] -> IO ()
printMsgs msgs = unless (null msgs) $ putStrLn (unlines msgs)

printCore :: String -> [CoreBind] -> IO ()
printCore msg core = do
    putStrLn $ msg ++ ":\n"
    mapM_ (printDump . ppr) core
    putStrLn "\n"

debugName :: (Var,CoreExpr) -> IO ()
debugName (v,_) =
    putStrLn $ show v ++ ":" ++
        "\n\tisId: " ++ show (isId v) ++
        "\n\tisLocalVar: " ++ show (isLocalVar v) ++
        "\n\tisLocalId: " ++ show (isLocalId v) ++
        "\n\tisGlobalId: " ++ show (isGlobalId v) ++
        "\n\tisExportedId: " ++ show (isExportedId v)

processFile :: Params -> FilePath -> IO ()
processFile params@Params{..} file = do

    putStrLn $ "Visiting " ++ file

    -- Get the initial core through Halo

    let dsconf = DesugarConf
                     { debug_float_out = db_float_out
                     , core2core_pass  = not no_core_optimise
                     }

    (modguts,dflags) <- desugar dsconf file

    let core_binds = mg_binds modguts

    when dump_init_core (printCore "Original core" core_binds)
    when db_names $ mapM_ debugName (flattenBinds core_binds)

    -- Lambda lift using GHC's lambda lifter. This also runs simpleCoreOptExpr

    floated_prog <- lambdaLift dflags core_binds

    when dump_float_out (printCore "Lambda lifted core" floated_prog)

    -- Case-/let- lift using Halo's lifter, also lift remaining lambdas

    us <- mkSplitUniqSupply 'c'

    let ((lifted_prog,msgs_lift),us2) = caseLetLift floated_prog us

    when db_lift          (printMsgs msgs_lift)
    when dump_lifted_core (printCore "Final, case/let lifted core" lifted_prog)

    -- Run our inliner

    let (inlined_prog,inline_kit) = inlineProgram lifted_prog
        InlineKit{..} = inline_kit

    when db_inliner $
        forM_ (flattenBinds lifted_prog) $ \(v,e) -> do
            putStrLn $ "Inlineable: " ++ show (varInlineable v)
            putStrLn $ "Before inlining:"
            putStrLn $ show v ++ " = " ++ showExpr e
            putStrLn $ "After inlining:"
            putStrLn $ show v ++ " = " ++ showExpr (inlineExpr e)
            putStrLn ""

    when dump_inlined_core (printCore "Inlined core" inlined_prog)

    -- Collect contracts

    let (collect_either_res,us3) = initUs us2 (collectContracts inlined_prog)

    (stmts,program,msgs_collect_contr) <- case collect_either_res of
        Right res -> return res
        Left err  -> putStrLn err >> exitFailure

    when db_collect (printMsgs msgs_collect_contr)
    when dump_contracts (mapM_ print stmts)

    when dump_final_core (printCore "Final core without Statements" program)

    -- Translate contracts & definitions

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

        ((fix_prog,fix_info),_us4)
            = initUs us3 (fixpointCoreProgram program)

        halo_env_without_hyp_arities
            = mkEnv halo_conf ty_cons_with_builtin fix_prog

        halo_env = halo_env_without_hyp_arities
            { arities = fpiFixHypArityMap fix_info
                            (arities halo_env_without_hyp_arities)
            }

        ((binds_thy,binds_map),msgs_trans) = runHaloM halo_env (trBinds fix_prog)

        background_thy = backgroundTheory halo_conf ty_cons_with_builtin
                      ++ map (mkDummySubtheory . Function) (fpiHypVars fix_info)

        subtheories
            = primConAxioms : primConApps : mkCF ty_cons_with_builtin
            ++ map makeDataDepend (binds_thy ++ background_thy)

    when dump_fpi_core (printCore "Fixpoint induction core" fix_prog)
    when db_halo       (printMsgs msgs_trans)

    let style_conf = StyleConf
            { style_comments   = comments
            , style_cnf        = not fof
            , style_dollar_min = dollar_min
            }

        toTPTP extra_clauses
            = (if quick_tptp then dumpTPTP
                    else linStrStyleTPTP style_conf . renameClauses)
            . (min_as_not_unr ? map minAsNotUnr)
            . (no_min ? removeMins)
            . (++ extra_clauses)
            . concatMap toClauses

    when dump_tptp $ putStrLn (toTPTP [] subtheories)

    let specialised_trim = trim subtheories

    forM_ stmts $ \top_stmt@TopStmt{..} -> do
        let (conjectures,msgs_tr_contr) = runHaloM halo_env $
                runReaderT (trTopStmt top_stmt) (TrEnv params fix_info binds_map)

        when db_trans (printMsgs msgs_tr_contr)

        forM_ conjectures $ \Conjecture{..} -> do

            let important    = Specific PrimConAxioms:Data boolTyCon:
                               conj_dependencies
                subtheories' = specialised_trim important

                tptp = toTPTP conj_clauses subtheories'

                filename = show top_name ++ conjKindSuffix conj_kind ++ ".tptp"

            whenNormal $ putStrLn $ "Writing " ++ show filename

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
