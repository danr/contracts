{-# LANGUAGE RecordWildCards #-}
{-

    "Pipe" a problem to paradox with some timeout and print the result

-}
module Contracts.Models.Pipe where

import Type

import Contracts.Models.Show
import Contracts.Models.TypeType ()
import Contracts.Models.ParadoxParser

import Contracts.Params

import System.Process
import System.IO
import System.Exit
import Control.Exception
import Control.Concurrent

import Data.Map (Map)

import Control.Monad

pipe :: Params -> Bool -> Map String Type -> FilePath -> IO ()
pipe Params{..} no_pointer_skolem_info ty_env f = do
    result <- runParadox paradox_timeout f
    case result of
        Just raw_model -> do
            when print_raw_model $ putStrLn (raw_model ++ "\n" ++ f)
            let model = parseParadoxModel raw_model
            putStrLn (showModel ignore_types no_pointer_skolem_info
                                typed_metas ty_env model)
        Nothing ->
            putStrLn $ "Killed paradox after " ++
                show paradox_timeout ++ " seconds."

runParadox :: Int -> String -> IO (Maybe String)
runParadox t file = timed t "paradox" ["--no-progress","--model",file]

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
