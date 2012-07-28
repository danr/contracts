{-# LANGUAGE QuasiQuotes #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import System.Environment
import System.Process
import System.ShQQ
import System.Timeout

readEnv :: String -> IO (Maybe String)
readEnv s = fmap snd . find ((s ==) . fst) <$> getEnvironment

splitPrefix :: String -> String
splitPrefix = reverse . dropWhile isDigit . drop 1 . dropWhile (/= '.') . reverse

data Res = SAT | UNSAT deriving (Eq,Ord,Show)

parseRes :: String -> Res
parseRes s
    | any (`isInfixOf` s) (words "sat broken ef fails oops") = SAT
    | otherwise = UNSAT

parseOutput :: String -> Maybe Res
parseOutput s
    | "Unsatisfiable" `isInfixOf` s = Just UNSAT
    | "Satisfiable"   `isInfixOf` s = Just SAT
    | otherwise = Nothing

timed :: Int -> IO String -> IO (Maybe Res)
timed t m = (parseOutput =<<) <$> timeout (t * 1000 * 1000) m
-- timed t m = parseOutput <$> m

runKoentool :: String -> Int -> String -> IO (Maybe Res)
runKoentool tool t file = timed t
    [sh| $tool --no-progress --tstp --time $t $file | grep RESULT |]

runEquinox = runKoentool "equinox"
runParadox = runKoentool "paradox"

runVampire t file = timed t
    [sh| timeout $t vampire_lin32 --mode casc -t $t < $file | grep status |]

runEprover t file = timed t
    [sh| timeout $t eprover -tAuto -xAuto --tptp3-format --cpu-limit=$t -s $file | grep status |]

runZ3 t file = do
    let regexp = "s/\\$min/min/g"
    [sh| sed $regexp $file > ${file}.z3 |]
    timed t [sh| timeout 1 z3 -tptp -nw ${file}.z3 | grep status |]

equinox = ("equinox",runEquinox)
paradox = ("paradox",runParadox)
vampire = ("vampire",runVampire)
eprover = ("eprover",runEprover)
z3      = ("z3",runZ3)

main = do

    -- Remove all tptp files
    system "find -iname '*.tptp' -exec rm -v {} +"
    system "find -iname '*.tptp.z3' -exec rm -v {} +"

    -- Get files from command line
    hs_files <- unwords . delete "RunTests.hs" . delete "Contracts.hs" <$> getArgs

    -- Generate all contracts
    system $ "hcc -d -i -b -C " ++ hs_files ++ " +RTS -prof"

    timeout <- maybe 1 read <$> readEnv "TIMEOUT"

    -- Find files, ensure that they are sorted
    files <- words <$> [sh| find -iname '*.tptp' |]

    let file_groups = groupBy ((==) `on` splitPrefix) (sort files)

    forM_ file_groups $ \group@(first:_) -> do

        let group_res = parseRes first

            group_size = length group

        -- We skip SAT groups
        unless (group_size > 1 && group_res == SAT) $ do

            putStrLn ""
            putStrLn $ first
                  ++ (guard (group_size > 1) >> (", group size: " ++ show group_size))
                  ++ ", should be " ++ show group_res ++ ":"

            -- If SAT, put paradox first, otherwise last
            let provers = case group_res of { SAT -> (paradox:) ; UNSAT -> (++ [paradox]) }
                        $ [z3,vampire,equinox,eprover]

            -- Run all files in the group, collect their [Maybe Res]
            results <- forM group $ \file -> do

                let put | group_size > 1 = putStrLn . ("    "++)
                        | otherwise      = putStrLn

                when (group_size > 1) (put file)

                -- Run until we get a result from a tool
                m_result <- unfoldM provers $ \(tool,prover) -> do
                    m_res <- prover 1 file
                    case m_res of
                        Nothing -> put $ tool ++ " timed out"
                        Just res -> put $ show res ++ " from " ++ tool
                    return m_res

                -- Interpret the result
                case m_result of
                    Nothing -> put "All tools timed out"
                    Just SAT | group_res == UNSAT -> printFail
                    Just SAT | group_res == SAT -> put "Success!"
                    _ -> return ()

                return m_result

            let just_results = catMaybes results

            -- If a SAT group couldn't be satisfied anywhere, it's a failure
            when (group_res == SAT && all (UNSAT ==) just_results && not (null just_results)) printFail

            -- If an UNSAT group was UNSAT everywhere, it's a success
            when (group_res == UNSAT && all (Just UNSAT ==) results) $ putStrLn "Success!"

printFail :: IO ()
printFail = putStrLn $ "=== FAIL ==="

unfoldM :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
unfoldM []     _ = return Nothing
unfoldM (x:xs) f = f x >>= \fx -> case fx of
    Nothing -> unfoldM xs f
    just    -> return just
