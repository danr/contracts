{-# LANGUAGE QuasiQuotes,RecordWildCards,NamedFieldPuns #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Concurrent
import Control.DeepSeq
import Control.Exception as C
import Data.Char
import Data.Function
import Data.List
import Data.Monoid
import Data.Maybe
import System.Environment
import System.Process
import System.FilePath
import System.IO
import System.Exit
import System.CPUTime
import System.Directory
import Text.Printf

readEnv :: String -> IO (Maybe String)
readEnv s = fmap snd . find ((s ==) . fst) <$> getEnvironment

splitPrefix :: String -> String
splitPrefix = reverse . dropWhile isDigit . drop 1 . dropWhile (/= '.') . reverse

data Res
    = SAT   { res_time :: Integer }
    | UNSAT { res_time :: Integer }

sat = SAT (-1)
unsat = UNSAT (-1)

isSAT,isUNSAT :: Res -> Bool
isSAT SAT{} = True
isSAT _     = False

isUNSAT = not . isSAT

showTime :: Integer -> String
-- showTime i = printf "%.1fms" ((fromInteger i :: Double) / (1000 * 1000 * 1000))
showTime i = show (i `div` (1000 * 1000 * 1000)) ++ "ms"

maybeShowTime :: Integer -> String
maybeShowTime (-1) = ""
maybeShowTime i    = " (" ++ showTime i ++ ")"

instance Show Res where
    show (SAT t)   = "SAT" ++ maybeShowTime t
    show (UNSAT t) = "UNSAT" ++ maybeShowTime t

instance Eq Res where
    SAT{}   == SAT{}   = True
    UNSAT{} == UNSAT{} = True
    _       == _       = False

parseRes :: String -> Res
parseRes s
    | any (`isInfixOf` s) (concatMap underscores $ words "sat broken ef fails oops") = sat
    | otherwise = unsat
  where
    underscores s = ["_"++s,s++"_"]

runKoentool :: String -> Int -> String -> IO (Maybe Res)
runKoentool tool t file = timed t tool Nothing $
    ["--no-progress","--tstp",file,"--time",show t]

runEquinox = runKoentool "equinox"
runParadox = runKoentool "paradox"

runVampire t file = do
    timed t "vampire_lin32" (Just file) $ ["--mode","casc","-t",show t]

runEprover t file = timed t "eprover" Nothing
    ["-tAuto","-xAuto","--tptp3-format","-s",file,"--cpu-limit="++show t]

--runZ3 t file = do
--    let regexp = "s/\\$min/min/g"
--    system $ "sed " ++ regexp ++ " " ++ show file ++ " > " ++ show file ++ ".z3"
--    timed t "z3" Nothing ["-tptp","-nw",file ++ ".z3","-t:"++show t]

runZ3smt t file = do
    let file' = dropExtension file ++ ".smt"
    exists <- doesFileExist file'
    if exists
        then timed t "z3" Nothing ["-smt2","-nw",file',"-t:"++show t]
        else do
            putStrLn $ file' ++ " does not exist!"
            return Nothing

equinox = ("xequinox",runEquinox)
paradox = ("paradox",runParadox)
vampire = ("vampire",runVampire)
eprover = ("eprover",runEprover)
--z3      = ("z3",runZ3)
z3smt   = ("smt-z3",runZ3smt)

unsat_provers = [z3smt,vampire,equinox,eprover]

getProvers :: Maybe String -> Res -> [(String,Int -> String -> IO (Maybe Res))]
getProvers (Just s) _ = [ p | c <- s
                        , Just p <- [find ((c ==) . head . fst) (unsat_provers ++ [paradox])]
                        ]
getProvers Nothing group_res = case group_res of
    { SAT{} -> (paradox:)
    ; UNSAT{} -> (++ [paradox])
    } $ unsat_provers

main = do

    hSetBuffering stdout LineBuffering

    -- Remove all tptp files
    system "find -iname '*.tptp' -exec rm {} +"
    system "find -iname '*.tptp.z3' -exec rm {} +"

    -- Get files from command line
    hs_files <- unwords . (\\ ["RunTests.hs","Contracts.hs","ResultSummary.hs"])
            <$> getArgs

    -- Profile if PROFILE env variable is set
    -- generate tptp with pretty names if READABLE is set
    -- use optimisation if OPTIMISE is set
    -- don't write a lot of output if QUIET is set
    -- don't use min if MIN=false
    profile     <- isJust <$> readEnv "PROFILE"
    readable    <- isJust <$> readEnv "READABLE"
    opt         <- isJust <$> readEnv "OPTIMISE"
    models      <- isJust <$> readEnv "MODELS"
    typed_metas <- isJust <$> readEnv "TYPED_METAS"
    timing      <- isJust <$> readEnv "TIMING"
    only        <- readEnv "ONLY"
    quiet       <- (timing ||) . isJust <$> readEnv "QUIET"
    no_min      <- (== Just "false") <$> readEnv "MIN"
    provers     <- do
        r <- readEnv "PROVERS"
        return $ case r of
            Nothing | timing -> Just "zevxp"
            _                -> r

    -- Use 1s timeout, or read from TIMEOUT env variable
    timeout <- maybe 1 read <$> readEnv "TIMEOUT"

    -- extra arguments to hcc
    hcc_args <- fromMaybe "" <$> readEnv "HCC_ARGS"

    let init_env = Env{..}

    -- print init_env

    unless (null hcc_args || quiet) $ putStrLn $ "HCC_ARGS: " ++ hcc_args

    -- Generate all contracts
    system $ "hcc --dollar-min --fpi-no-base --fpi-no-plain --fpi-split "
                ++ (guard quiet >> " -q")
                ++ (if readable then " --comments " else " --quick-tptp ")
                ++ (guard no_min >> " --no-min ")
                ++ (guard opt >> " --core-optimise ")
                ++ " " ++ hcc_args ++ " "
                ++ hs_files
                ++ (guard profile >> " +RTS -prof")

    -- Find files, ensure that they are sorted
    files <- words <$> readProcess "find" ["-iname","*.tptp"] ""

    let file_groups = groupBy ((==) `on` splitPrefix) (sort files)

    when timing $ putStrLn $ csv $
        "filename":map fst (getProvers provers
            $ error "getProvers, timing, set PROVERS")

    Out{..} <- execWriterT $ (`runReaderT` init_env) $ mapM_ processGroup file_groups

    let no_fails = length failures
        no_tos   = length timeouts

    unless timing $ do
        putStrLn ""
        putStrLn $ "Timeouts: " ++ show no_tos
        when (no_tos > 0) $ do
            putStrLn $ "Files timing out: "
            mapM_ (putStrLn . ('\t':)) timeouts

        putStrLn ""
        putStrLn $ "Failures: " ++ show no_fails
        when (no_fails > 0) $ do
            printFail
            putStrLn $ "Failing files: "
            mapM_ (putStrLn . ('\t':)) failures


type M = ReaderT Env (WriterT Out IO)

data Out = Out
    { failures :: [FilePath]
    , timeouts :: [FilePath]
    }

data Env = Env
    { quiet       :: Bool
    , timeout     :: Int
    , models      :: Bool
    , typed_metas :: Bool
    , hcc_args    :: String
    , timing      :: Bool
    , only        :: Maybe String
    , provers     :: Maybe String
    }
  deriving Show

-- Verbose put
vput :: String -> M ()
vput s = do
    Env{quiet} <- ask
    unless quiet $ put s

-- Just put! (Unless we're timing, then we want to do something else)
put :: String -> M ()
put s = do
    Env{timing} <- ask
    unless timing . liftIO . putStrLn $ s

endl :: M ()
endl = put ""

processGroup :: [FilePath] -> M ()
processGroup group@(first:_) = do
    let group_res = parseRes first
        group_size = length group

    Env{models,timing,only} <- ask

    let pursue0 | models    = group_res == sat && group_size == 1
                | otherwise = group_res == unsat || group_size == 1
        pursue = pursue0 && ((group_res == sat && only /= Just "UNSAT") ||
                             (group_res == unsat && only /= Just "SAT"))

    when pursue $ do

        let file_desc = takeFileName first

        endl
        put $ file_desc
              ++ (guard (group_size > 1) >> (", group size: " ++ show group_size))
              ++ ", should be " ++ show group_res

        -- Run all files in the group, collect their [[Maybe Res]]
        results <- mapM (processFile group_res group_size) group

        if timing
            then do
                -- get the result per prover... combine if it comes from a group
                let results_per_prover = map combineMRes (transpose results)
                liftIO $ putStrLn $ csv (file_desc:map showMRes results_per_prover)
            else do
                let results' = map msum results
                    just_results = catMaybes results'

                -- If a SAT group couldn't be satisfied anywhere, it's a failure
                when (group_res == sat && all (unsat ==) just_results && not (null just_results)) $ do
                    regFailure first
                    printFail

                -- If an UNSAT group was UNSAT everywhere, it's a success
                when (group_res == unsat && all (Just unsat ==) results') $ put "Success!"

csv :: [String] -> String
csv = intercalate ","

showMRes :: Maybe Res -> String
showMRes Nothing  = "---"
showMRes (Just r) = showTime (res_time r)

combineMRes :: [Maybe Res] -> Maybe Res
combineMRes = foldr1 combineTwoMRes

combineTwoMRes :: Maybe Res -> Maybe Res -> Maybe Res
combineTwoMRes = liftM2 combineTwoRes

combineTwoRes :: Res -> Res -> Res
combineTwoRes (UNSAT t1) (UNSAT t2) = UNSAT (t1 + t2)
combineTwoRes (SAT t1)   (UNSAT t2) = SAT (t1 + t2)
combineTwoRes (UNSAT t1) (SAT t2)   = SAT (t1 + t2)
combineTwoRes (SAT t1)   (SAT t2)   = SAT (t1 + t2)

processFile :: Res -> Int -> FilePath -> M [Maybe Res]
processFile group_res group_size file_init = do

    let file = takeFileName file_init

    Env{..} <- ask

    if models
        then do
            let hs_file = takeWhile (/= '.') file ++ ".hs"

                cmd = "hcc " ++ hs_file ++ " --paradox-file=" ++ file
                         ++ (guard typed_metas >> " --typed-metas")
                         ++ " " ++ hcc_args

            liftIO $ do
                putStrLn file
                putStrLn hs_file
                putStrLn cmd
                system cmd

            return $ [Just sat]
        else do

            let iput | group_size > 1 = vput . ("    "++)
                     | otherwise      = vput

            when (group_size > 1) (vput $ takeFileName file)

            -- Run until we get a result from a tool, or if we're timing, run all
            results <- breakAtJustOrNot (not timing) (getProvers provers group_res) $
                \(tool_desc,prover) -> do
                    m_res <- liftIO $ prover timeout file
                    case m_res of
                        Nothing  -> iput $ tool_desc ++ " timed out"
                        Just res -> iput $ show res ++ " from " ++ tool_desc
                    return m_res

            -- Interpret the result
            unless timing $ case msum results of
                Nothing -> regTimeout file >> iput "All tools timed out"
                Just SAT{} | group_res == unsat -> regFailure file >> printFail
                Just SAT{} | group_res == sat   -> iput "Success!"
                _ -> return ()

            return results

printFail :: MonadIO m => m ()
printFail = do
    liftIO $ putStrLn "=============================================================================="
    liftIO $ putStrLn "!!! FAIL                                                                   !!!"
    liftIO $ putStrLn "=============================================================================="

breakAtJustOrNot :: Monad m => Bool -> [a] -> (a -> m (Maybe b)) -> m [Maybe b]
breakAtJustOrNot True  = untilJust
breakAtJustOrNot False = forM

untilJust :: Monad m => [a] -> (a -> m (Maybe b)) -> m [Maybe b]
untilJust []     _ = return []
untilJust (x:xs) f = do
    fx <- f x
    case fx of
        Nothing -> (fx:) `liftM` untilJust xs f
        just    -> return [just]

instance Monoid Out where
    mempty = Out [] []
    mappend (Out u v) (Out s t) = Out (u ++ s) (v ++ t)

regFailure :: FilePath -> M ()
regFailure f = tell (Out [f] [])

regTimeout :: FilePath -> M ()
regTimeout f = tell (Out [] [f])

-- adapted from HipSpec.ATP.RunProver
timed :: Int -> FilePath -> (Maybe FilePath) -> [String] -> IO (Maybe Res)
timed t cmd inf args = errHandle $ do

    t0 <- getCPUTime
    (Just inh, Just outh, Just errh, pid) <-
         createProcess (proc cmd args)
                       { std_in  = CreatePipe
                       , std_out = CreatePipe
                       , std_err = CreatePipe }

    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    void $ forkIO $ evaluate (length output) >> putMVar outMVar ()

    void $ forkIO $ do
        err <- hGetContents errh
        n   <- evaluate (length err)
        when (n > 0) $ hPutStrLn stderr $
            "*** " ++ cmd ++ " stderr: ***" ++ "\n" ++ err

    case inf of
        Just file -> do
            inp <- readFile file
            hPutStr inh inp
            hFlush inh
        Nothing -> return ()

    hClose inh

    exit_code_mvar <- newEmptyMVar

    tid <- forkIO $ do
         -- read output
         takeMVar outMVar
         -- wait on the process
         ex <- waitForProcess pid
         t1 <- getCPUTime
         hClose outh
         putMVar exit_code_mvar (Just (ex,t1-t0))

    kid <- forkIO $ do
         threadDelay (t * 1000 * 1000)
         killThread tid
         terminateProcess pid
         void $ waitForProcess pid
         putMVar exit_code_mvar Nothing

    maybe_exit_code <- takeMVar exit_code_mvar

    killThread tid
    killThread kid

    return $ do
        (ExitSuccess,t) <- maybe_exit_code
        parseOutput output t

  where

    errHandle m = m `C.catch` \e -> do
        hPutStrLn stderr (show (e :: IOException))
        return Nothing

parseOutput :: String -> Integer -> Maybe Res
parseOutput s t
    | "Unsatisfiable" `isInfixOf` s = Just $ UNSAT t
    | "Satisfiable"   `isInfixOf` s = Just $ SAT t
    | "unsat"         `isInfixOf` s = Just $ UNSAT t
    | "sat"           `isInfixOf` s = Just $ SAT t
    | otherwise = Nothing

