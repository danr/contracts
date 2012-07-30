{-# LANGUAGE QuasiQuotes,RecordWildCards,NamedFieldPuns #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Data.Char
import Data.Function
import Data.List
import Data.Monoid
import Data.Maybe
import System.Environment
import System.Process
import System.FilePath
import System.ShQQ
import System.Process
import System.IO
import System.Exit
import System.CPUTime
import System.Directory

readEnv :: String -> IO (Maybe String)
readEnv s = fmap snd . find ((s ==) . fst) <$> getEnvironment

splitPrefix :: String -> String
splitPrefix = reverse . dropWhile isDigit . drop 1 . dropWhile (/= '.') . reverse

data Res = SAT | UNSAT deriving (Eq,Ord,Show)

parseRes :: String -> Res
parseRes s
    | any (`isInfixOf` s) (concatMap underscores $ words "sat broken ef fails oops") = SAT
    | otherwise = UNSAT
  where
    underscores s = ["_"++s,s++"_"]

parseOutput :: String -> Maybe Res
parseOutput s
    | "Unsatisfiable" `isInfixOf` s = Just UNSAT
    | "Satisfiable"   `isInfixOf` s = Just SAT
    | otherwise = Nothing

runKoentool :: String -> Int -> String -> IO (Maybe Res)
runKoentool tool t file = timed t tool "" $
    ["--no-progress","--tstp",file]

runEquinox = runKoentool "equinox"
runParadox = runKoentool "paradox"

runVampire t file = do
    inp <- readFile file
    timed t "vampire" inp $ ["--mode casc"]

runEprover t file = timed t "eprover" ""
    ["-tAuto","-xAuto","--tptp3-format","-s",file]

runZ3 t file = do
    let regexp = "s/\\$min/min/g"
    [sh| sed $regexp $file > ${file}.z3 |]
    timed t "z3" "" ["-tptp","-nw",file ++ ".z3"]

equinox = ("equinox",runEquinox)
paradox = ("paradox",runParadox)
vampire = ("vampire",runVampire)
eprover = ("eprover",runEprover)
z3      = ("z3",runZ3)

main = do

    -- Remove all tptp files
    system "find -iname '*.tptp' -exec rm {} +"
    system "find -iname '*.tptp.z3' -exec rm {} +"

    -- Get files from command line
    hs_files <- unwords . delete "RunTests.hs" . delete "Contracts.hs" <$> getArgs

    -- Profile if PROFILE env variable is set
    -- generate tptp with pretty names if READABLE is set
    -- don't write a lot of output if QUIET is set
    -- don't use min if MIN=false
    profile  <- isJust <$> readEnv "PROFILE"
    readable <- isJust <$> readEnv "READABLE"
    quiet    <- isJust <$> readEnv "QUIET"
    no_min   <- (== Just "false") <$> readEnv "MIN"

    -- Use 1s timeout, or read from TIMEOUT env variable
    timeout <- maybe 1 read <$> readEnv "TIMEOUT"

    let init_env = Env quiet timeout

    -- Generate all contracts
    system $ "hcc -q --dollar-min --fpi-no-base --fpi-no-plain "
                ++ (guard (not readable) >> " --quick-tptp ")
                ++ (guard no_min >> " --no-min ")
                ++ hs_files
                ++ (guard profile >> " +RTS -prof")

    -- Find files, ensure that they are sorted
    files <- words <$> [sh| find -iname '*.tptp' |]

    let file_groups = groupBy ((==) `on` splitPrefix) (sort files)

    Out{..} <- execWriterT $ (`runReaderT` init_env) $ mapM_ processGroup file_groups

    let no_fails = length failures
        no_tos   = length timeouts

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
    { quiet   :: Bool
    , timeout :: Int
    }

put :: String -> M ()
put s = do
    Env{quiet} <- ask
    unless quiet . liftIO . putStrLn $ s

endl :: M ()
endl = put ""

processGroup :: [FilePath] -> M ()
processGroup group@(first:_) = do
    let group_res = parseRes first
        group_size = length group

    -- We skip SAT groups
    unless (group_size > 1 && group_res == SAT) $ do

        endl
        liftIO $ putStrLn $ takeFileName $ first
              ++ (guard (group_size > 1) >> (", group size: " ++ show group_size))
              ++ ", should be " ++ show group_res

        -- Run all files in the group, collect their [Maybe Res]
        results <- mapM (processFile group_res group_size) group

        let just_results = catMaybes results

        -- If a SAT group couldn't be satisfied anywhere, it's a failure
        when (group_res == SAT && all (UNSAT ==) just_results && not (null just_results)) $ do
            regFailure first
            printFail

        -- If an UNSAT group was UNSAT everywhere, it's a success
        when (group_res == UNSAT && all (Just UNSAT ==) results) $ put "Success!"

processFile :: Res -> Int -> FilePath -> M (Maybe Res)
processFile group_res group_size file = do

    Env{timeout} <- ask

    -- If SAT, put paradox first, otherwise last
    let provers = case group_res of { SAT -> (paradox:) ; UNSAT -> (++ [paradox]) }
                $ [z3,vampire,equinox,eprover]

        put' | group_size > 1 = put . ("    "++)
             | otherwise      = put

    when (group_size > 1) (put $ takeFileName file)

    -- Run until we get a result from a tool
    m_result <- unfoldM provers $ \(tool,prover) -> do
        m_res <- liftIO $ prover timeout file
        case m_res of
            Nothing  -> put' $ tool ++ " timed out"
            Just res -> put' $ show res ++ " from " ++ tool
        return m_res

    -- Interpret the result
    case m_result of
        Nothing -> regTimeout file >> put "All tools timed out"
        Just SAT | group_res == UNSAT -> regFailure file >> printFail
        Just SAT | group_res == SAT   -> put' "Success!"
        _ -> return ()

    return m_result

printFail :: MonadIO m => m ()
printFail = do
    liftIO $ putStrLn "=============================================================================="
    liftIO $ putStrLn "!!! FAIL                                                                   !!!"
    liftIO $ putStrLn "=============================================================================="

unfoldM :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
unfoldM []     _ = return Nothing
unfoldM (x:xs) f = f x >>= \fx -> case fx of
    Nothing -> unfoldM xs f
    just    -> return just

instance Monoid Out where
    mempty = Out [] []
    mappend (Out u v) (Out s t) = Out (u ++ s) (v ++ t)

regFailure :: FilePath -> M ()
regFailure f = tell (Out [f] [])

regTimeout :: FilePath -> M ()
regTimeout f = tell (Out [] [f])

-- adapted from HipSpec.ATP.RunProver
timed :: Int -> FilePath -> String -> [String] -> IO (Maybe Res)
timed t cmd inp args  = do

    (Just inh, Just outh, Just errh, pid) <-
         createProcess (proc cmd args)
                       { std_in  = CreatePipe
                       , std_out = CreatePipe
                       , std_err = CreatePipe }

    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    void $ forkIO $ rnf output `seq` putMVar outMVar ()

    void $ forkIO $ do
        err <- hGetContents errh
        n   <- evaluate (length err)
        when (n > 0) $ hPutStrLn stderr $
            "*** " ++ cmd ++ " stderr: ***" ++ "\n" ++ err

    unless (null inp) $ do
        hPutStr inh inp
        hFlush inh

    hClose inh

    exit_code_mvar <- newEmptyMVar

    tid <- forkIO $ do
         -- read output
         takeMVar outMVar
         -- wait on the process
         ex <- rnf output `seq` waitForProcess pid
         hClose outh
         putMVar exit_code_mvar ex

    kid <- forkIO $ do
         threadDelay (t * 1000 * 1000)
         terminateProcess pid
         putMVar exit_code_mvar =<< waitForProcess pid

    maybe_exit_code <- takeMVar exit_code_mvar

    killThread tid
    killThread kid

    return $ case maybe_exit_code of
                ExitSuccess -> parseOutput output
                _           -> Nothing
