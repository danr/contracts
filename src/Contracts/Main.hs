{-# LANGUAGE RecordWildCards, ViewPatterns, DisambiguateRecordFields, PatternGuards #-}
{-

        888
        888
        888
        888888b.  .d8888  .d8888
        888  888 d8P     d8P
        888  888 888     888
        888  888 Y8b.    Y8b.
        888  888  "Y8888  "Y8888
        haskell contracts checker

    The - admittedly oversized - entry point to the contracts checker.

    The pipeline is approximately this:

        * Get CoreBinds from a file via the GHC API,

        * Find /unfoldings/ from all unknown global identifiers from
          other modules and make CoreBinds from them,

        * Find all used data types from used constructors,

        * Lambda lift using GHC's lambda lifter,

        * Case/let lift, and also lift remaining lambdas,

        * Remove DEFAULT alternatives,

        * Inline all non-recursive, non-casing definitions without
          introducing any lambdas. Exception: after HOAS constructs
          like (:->), Pred and All, it is OK to introduce lambdas,

        * Internalise statements, i.e. go from Core -> internal representation,

        * Make fixed point induction versions of recursive functions,

        * Translate definitions to FOL,

        * Translate internal representation of statements to FOL,
          possibly chopped up in separate goals,

        * Make as minimal theories as possible for each goal,
          and save them to tptp-files.

        * The paradox-pretty printer can be run, with type information.

-}
module Main where

import Class
import CoreSyn
import GHC
import HscTypes
import Outputable
import TysWiredIn
import UniqSupply
import Var

-- To debug TyCons
import TyCon
import Id
import DataCon

-- To debug names
import CoreFVs
import VarSet

import Halo.BackgroundTheory
import Halo.Binds
import Halo.Conf
import Halo.Class
import Halo.Entry
import Halo.Fetch
import Halo.FOL.Abstract
import Halo.FOL.Linearise
import Halo.FOL.Dump
import Halo.FOL.LineariseSMT
import Halo.FOL.MinAsNotUnr
import Halo.FOL.RemoveMin
import Halo.FOL.Rename
import Halo.FreeTyCons
import Halo.Lift
import Halo.Monad
import Halo.RemoveDefault
import Halo.Shared
import Halo.Subtheory
import Halo.Trim
import Halo.Util

import Contracts.Models.Pipe

import Contracts.Axioms
import Contracts.Internalise
import Contracts.FixpointInduction
import Contracts.Inliner
import Contracts.Params
import Contracts.HCCTheory
import Contracts.Translate
import Contracts.InternalRepr
import Contracts.SourceRepr

import Control.Monad
import Control.Monad.Reader

import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

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
debugName (v,e) =
    putStrLn $ show v ++ ":" ++
        "\n\tisId: " ++ show (isId v) ++
        "\n\tisLocalVar: " ++ show (isLocalVar v) ++
        "\n\tisLocalId: " ++ show (isLocalId v) ++
        "\n\tisGlobalId: " ++ show (isGlobalId v) ++
        "\n\tisExportedId: " ++ show (isExportedId v) ++
        "\n\tidUnfolding: " ++ showOutputable (idUnfolding v) ++
        "\n\tfreeVars: " ++ showOutputable (exprFreeVars e) ++
        "\n\tglobal free vars: " ++
            showOutputable (exprSomeFreeVars isGlobalId e) ++
        "\n\tglobal free vars unfoldings: " ++
            (showOutputable . map (\x -> (x,idUnfolding x))
            . varSetElems . exprSomeFreeVars isGlobalId $ e) ++
        "\n\tglobal free vars real unfoldings: " ++
            (showOutputable . map (\x -> (x,realIdUnfolding x))
            . varSetElems . exprSomeFreeVars isGlobalId $ e) ++
        "\n\tglobal free vars unfolding expressions: " ++
            (showOutputable
            . map (\x -> (x,maybeUnfoldingTemplate (idUnfolding x)))
            . varSetElems . exprSomeFreeVars isGlobalId $ e) ++
        "\n\tglobal free vars real unfolding expressions: " ++
            (showOutputable
            . map (\x -> (x,maybeUnfoldingTemplate (realIdUnfolding x)))
            . varSetElems . exprSomeFreeVars isGlobalId $ e)

processFile :: Params -> FilePath -> IO ()
processFile params@Params{..} file = do

    whenNormal $ putStrLn $ "Visiting " ++ file

    -- Get the initial core through Halo

    let dsconf = DesugarConf
                     { debug_float_out = db_float_out
                     , core2core_pass  = core_optimise
                     }

    ((modguts,type_env),dflags) <- desugarAndTypeEnv dsconf file

    let init_core_binds = mg_binds modguts

    when dump_init_core (printCore "Original core" init_core_binds)
    when db_names $ mapM_ debugName (flattenBinds init_core_binds)

    -- Find and print unfoldings

    let (unfoldings,debug_unfoldings) = fetch init_core_binds
        core_binds = unfoldings ++ init_core_binds

    when db_unfoldings (putStrLn debug_unfoldings)
    when dump_unfoldings (printCore "Additional core from unfoldings" unfoldings)

    -- Get type constructors

    let ty_cons :: [TyCon]
        ty_cons = filter (not . contractModuleName . tyConName)
                         (insert boolTyCon (fetchTyCons core_binds))

    -- Debug tycons

    when db_ty_cons $ do
        putStrLn $ "ty_cons: " ++ showOutputable ty_cons
        void $ sequence $
            [ putStrLn $ "ty_cons, tyConDataCons with " ++ t ++ " on " ++ s ++ ": "
               ++ showOutputable (map (map ((id &&& l) . k) . tyConDataCons) ty_cons)
            | (l,t) <- [(isConLikeId,"isConLikeId")
                       ,(isNewtypeConId,"isNewtypeConId")
                       ]
            , (k,s) <- [(dataConWorkId,"dataConWorkId")
                       ,(dataConWrapId,"dataConWrapId")
                       ]
            ]
        putStrLn $ "is newtype: " ++ showOutputable (map isNewTyCon ty_cons)

    -- Lambda lift using GHC's lambda lifter. This also runs simpleCoreOptExpr

    let classes = typeEnvClasses type_env

    when db_classes $ do
        putStrLn $ "Classes :" ++ showOutputable classes
        putStrLn $ "Class algTyConRhs: " ++
            showOutputable (map (data_cons . algTyConRhs . classTyCon) classes)

    floated_prog <- lambdaLift dflags (classBinds classes ++ core_binds)

    when dump_float_out (printCore "Lambda lifted core" floated_prog)

    -- Case-/let- lift using Halo's lifter, also lift remaining lambdas

    us0 <- mkSplitUniqSupply 'c'

    let ((lifted_prog,msgs_lift),us1) = caseLetLift floated_prog case_lift_inner us0

    when db_lift          (printMsgs msgs_lift)
    when dump_lifted_core (printCore "Case/let lifted core" lifted_prog)

    -- Remove DEFAULT alternatives

    let (rm_def_prog,us2) = initUs us1 (removeDefaults lifted_prog)

    when dump_rm_def_core (printCore "Core without DEFAULT alternatives" rm_def_prog)

    -- Run our inliner

    let (inlined_prog,inline_kit) = inlineProgram rm_def_prog
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

    -- Internalise contracts

    let (internalise_either_res,us3) = initUs us2 (internaliseContracts inlined_prog)

    (stmts,program,msgs_internalise_contr) <- case internalise_either_res of
        Right res -> return res
        Left err  -> putStrLn err >> exitFailure

    when db_internalise (printMsgs msgs_internalise_contr)
    when dump_contracts (mapM_ print stmts)

    when dump_final_core (printCore "Final core without Statements" program)

    -- Change recursive definitions for fixed point induction

    let ((fix_prog,fix_info),_us4)
            = initUs us3 (fixpointCoreProgram program)

    when dump_fpi_core (printCore "Fixpoint induction core" fix_prog)

    -- Translate definitions

    let halo_conf :: HaloConf
        halo_conf = sanitizeConf $ HaloConf
            { use_min           = not no_min
            , use_minrec        = False
            , unr_and_bad       = True
            , ext_eq            = False
            -- ^ False for now, no good story about min and ext-eq
            , or_discr          = or_discr
            , var_scrut_constr  = var_scrut_constr
            }

        halo_env_without_hyp_arities
            = mkEnv halo_conf ty_cons fix_prog

        halo_env = halo_env_without_hyp_arities
            { arities = fpiFixHypArityMap fix_info
                            (arities halo_env_without_hyp_arities)
            }

        ((binds_thy,binds_map),msgs_trans) = runHaloM halo_env (trBinds fix_prog)

        background_thy = backgroundTheory halo_conf ty_cons
                      ++ map (mkDummySubtheory . Function) (fpiHypVars fix_info)

        app_theory = Subtheory
            { provides    = AppTheory
            , depends     = [ Specific PrimConApps, AppOnMin ]
            , description = "This theory uses the app symbol"
            , formulae    = []
            }

        subtheories
            = primConAxioms params : primConApps : app_theory : mkCF ty_cons
            ++ map makeDataDepend (binds_thy ++ background_thy)

    when db_halo       (printMsgs msgs_trans)

    let style_conf = StyleConf
            { style_comments   = comments
            , style_cnf        = not fof
            , style_dollar_min = dollar_min
            }

        dumpTPTP' :: [VClause] -> (String,dump_error)
        dumpTPTP' x = (dumpTPTP x,error "Can't dump tptp and print model!")

        prep :: [VClause] -> [HCCSubtheory] -> [VClause]
        prep extra_clauses
            = (min_as_not_unr ? map minAsNotUnr)
            . (no_min ? removeMins)
            . (++ extra_clauses)
            . concatMap toClauses

        toTPTP :: [VClause] -> [HCCSubtheory] -> (String,Map String Var)
        toTPTP
            = (if quick_tptp
                   then dumpTPTP'
                   else first (linStrStyleTPTP style_conf) . renameClauses)
            .: prep

        toSMT :: [VClause] -> [HCCSubtheory] -> String
        toSMT = linSMT .: prep

    when dump_tptp . putStrLn . fst $ toTPTP [] subtheories

    when dump_subthys $ do
        putStrLn "All subtheories"
        mapM_ print subtheories

    let specialised_trim = trim subtheories

    forM_ stmts $ \top@TopStmt{..} -> do

        -- Translate contracts
        let (conjectures,msgs_tr_contr) = runHaloM halo_env $
                runReaderT (trTopStmt top) (TrEnv params fix_info binds_map)

        when db_contract_trans (printMsgs msgs_tr_contr)

        -- Assemble tptp and smt files

        forM_ conjectures $ \Conjecture{..} -> do

            let important    = Specific PrimConAxioms:Data boolTyCon:
                               conj_dependencies
                subtheories' = specialised_trim important

                filename ext = show top_name ++ conjKindSuffix conj_kind
                                ++ "." ++ ext

                tptp_file = filename "tptp"
                smt_file  = filename "smt"

                -- If printing models, we can make everything disjoint:
                -- a pass here that makes unrelated data types and
                -- function pointer disjoint from each other.

                disjoint_clauses
                    | all_disjoint = extraDisjoint halo_conf subtheories'
                    | otherwise    = []

                (tptp,rep_map) = toTPTP (disjoint_clauses ++ conj_clauses)
                                        subtheories'

                smt = toSMT (disjoint_clauses ++ conj_clauses) subtheories'

                write_files = do
                    unless no_tptp $ do
                        whenLoud $ putStrLn $ "Writing " ++ tptp_file
                        writeFile tptp_file tptp

                    unless (isJust paradox_file || no_smt) $ do
                        whenLoud $ putStrLn $ "Writing " ++ smt_file
                        writeFile smt_file smt

            when dump_subthys $ do
                putStrLn $ "Subtheories: "
                mapM_ print subtheories'

            case paradox_file of
                Just f | f == tptp_file -> do
                    write_files
                    putStrLn $ "Piping " ++ f ++ " to paradox, wish me luck!"
                    let env = M.map varType rep_map
                    pipe params env f
                _ -> write_files


main :: IO ()
main = do

    params@Params{..} <- sanitizeParams <$> cmdArgs defParams

    when (null files) $ do
        putStrLn "No input files!"
        putStrLn ""

    mapM_ (processFile params . dropExtension) files
