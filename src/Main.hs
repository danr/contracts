{-# LANGUAGE RecordWildCards #-}
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
import Halo.FOL.Rename
import Halo.FOL.Style
import Halo.Lift
import Halo.Monad
import Halo.Subtheory
import Halo.Trans
import Halo.Trim

import Contracts.Make
import Contracts.Trans
import Contracts.Types

import Control.Monad
import System.Environment
import System.Exit

main :: IO ()
main = do
    file:opts <- getArgs
    let flagged x = when (x `elem` opts)
        dsconf    = DesugarConf { debug_float_out = "-debug-float-out" `elem` opts
                                , core2core_pass  = False
                                }
    (modguts,dflags) <- desugar dsconf file
    let core_binds = mg_binds modguts

    (program,m_stmts) <-
         if "-only-translate" `elem` opts
             then return (core_binds,Nothing)
             else do
                 us <- mkSplitUniqSupply 'c'
                 let ((r,msgs),_us') = collectContracts us core_binds
                 flagged "-dbmkcontr" (mapM_ putStrLn msgs)
                 case r of
                      Right (stmts,bs) -> do flagged "-dbmkcontr" (mapM_ print stmts)
                                             return (bs,Just stmts)
                      Left err         -> do putStrLn err
                                             exitFailure

    floated_prog <- lambdaLift dflags program
    us <- mkSplitUniqSupply 'f'

    let ty_cons :: [TyCon]
        ty_cons = mg_tcs modguts

        ty_cons_with_builtin :: [TyCon]
        ty_cons_with_builtin = listTyCon : boolTyCon : unitTyCon
                             : [ tupleTyCon BoxedTuple size
                               | size <- [0..8]
                               -- ^ choice: only tuples of size 0 to 8 supported!!
                               ]
                             ++ ty_cons

        cnf = "-cnf" `elem` opts

        halt_conf :: HaloConf
        halt_conf  = sanitizeConf $ HaloConf
                        { use_min      = "-no-min" `notElem` opts
                        , use_cf       = True
                        , unr_and_bad  = True
                        }

        ((lifted_prog,msgs_lift),_us) = caseLetLift floated_prog us

        halt_env = mkEnv halt_conf ty_cons_with_builtin lifted_prog

        (subtheories,msgs_trans)
            = translate halt_env ty_cons_with_builtin lifted_prog

        printSrc = do
            putStrLn $ "Original file, " ++ file ++ ":\n"
            putStrLn =<< readFile (file ++ ".hs")

        printMsgs msgs = unless (null msgs) $ putStrLn $ unlines msgs

        endl = putStrLn "\n"

        printCore msg core = do
            putStrLn $ msg ++ ":\n"
            mapM_ (printDump . ppr) core
            endl

    flagged "-src-before" printSrc

    flagged "-origcore" (printCore "Original core" core_binds)

    flagged "-lamlift" (printCore "Lambda lifted core" floated_prog)

    flagged "-dbcaseletlift" (printMsgs msgs_lift)
    flagged "-caseletlift"   (printCore "Case/let lifted core" lifted_prog)

    flagged "-src" printSrc

    flagged "-dbtptp" (printMsgs msgs_trans)

    case m_stmts of
        Nothing -> do
             unless ("-no-tptp" `elem` opts) $ do
                 let tptp = linTPTP (strStyle cnf)
                                    (renameClauses (concatMap toClauses subtheories))
                 putStrLn tptp

        Just stmts -> forM_ stmts $ \stmt@(Statement{..}) -> do
             let ((tr_contract,deps),msgs_tr_contr) = runHaloM halt_env (trStatement stmt)
             flagged "-dbtrcontr" (printMsgs msgs_tr_contr)
             print statement_name
             let subtheories' = trimFuns halt_conf deps subtheories
                 tptp = linTPTP (strStyle cnf)
                                (renameClauses $ concatMap toClauses subtheories'
                                                 ++ tr_contract)
             putStrLn tptp
             writeFile (show statement_name ++ ".tptp") tptp

