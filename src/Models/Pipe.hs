-- "Pipe" a problem to paradox with some timeout and print the result
module Models.Pipe where

import Type

import Models.Show
import Models.TypeType ()
import Models.ParadoxParser

import Contracts.Params

import System.Process
import System.IO
import System.Exit
import Control.Exception
import Control.Concurrent

import Data.Map (Map)

import Control.Monad

pipe :: Params -> Map String Type -> FilePath -> IO ()
pipe params ty_env f = do
    result <- runParadox 20 f
    case result of
        Just raw_model -> do
            putStrLn raw_model
            let model = parseParadoxModel raw_model
            putStrLn (showModel (typed_metas params) ty_env model)
        Nothing -> putStrLn "Killed paradox after 20 seconds."

runParadox :: Int -> String -> IO (Maybe String)
runParadox t file = timed t "paradox" $
    ["--no-progress","--model",file]

timed :: Int -> FilePath -> [String] -> IO (Maybe String)
timed t cmd args = do

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

    hClose inh

    exit_code_mvar <- newEmptyMVar

    tid <- forkIO $ do
         -- read output
         takeMVar outMVar
         -- wait on the process
         ex <- waitForProcess pid
         hClose outh
         putMVar exit_code_mvar (Just ex)

    kid <- forkIO $ do
         threadDelay (t * 1000 * 1000)
         killThread tid
         terminateProcess pid
         void $ waitForProcess pid
         putMVar exit_code_mvar Nothing

    maybe_exit_code <- takeMVar exit_code_mvar

    killThread tid
    killThread kid

    return $ case maybe_exit_code of
        Just ExitSuccess -> Just output
        _                -> Nothing
