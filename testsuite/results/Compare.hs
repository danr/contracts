{-# LANGUAGE ViewPatterns #-}
module Main where

import Prelude hiding (compare)

import System.Environment

import Control.Applicative
import Control.Monad

import Data.List
import Data.Maybe
import Data.Either

import Text.Printf

main :: IO ()
main = compare =<< getArgs

compare :: [FilePath] -> IO ()
compare files = do

    tables <- forM files $ \f -> (,) f . tabulate <$> readFile f

    -- Total successes
    let successes :: [Res] -> Int
        successes = length . filter (any isJust)

        avg :: [Maybe Double] -> Double
        avg (catMaybes -> ts) = sum ts / genericLength ts

        putTables :: Bool -> [(FilePath,([Res],[Res]))] -> IO ()
        putTables print_avg = mapM_ $ \(file,(sat,uns)) -> do
            putStr (intercalate "\t" $ map (show . successes) [sat ++ uns,sat,uns])
            when print_avg $ do
                putStr "\t"
                putStr (intercalate "\t" $ map (printf "%.3f" . avg . map head) [sat ++ uns,sat,uns])
            putStrLn $ "\t" ++ file

    putStrLn $ "Total\tSAT\tUNS successes\tFilename"
    putTables False tables

    -- Foreach theorem prover

    let provers    = zip [0..] ["paradox","equinox","z3","vampire","eprover"]
        only :: Int -> [(a,([[b]],[[c]]))] -> [(a,([[b]],[[c]]))]
        only ix = map (\(a,(bs,cs)) -> (a,(cursor bs,cursor cs)))
          where
            cursor :: [[d]] -> [[d]]
            cursor = map (return . (!! ix))

    forM_ provers $ \(ix,prover) -> do
        putStrLn "\n"
        putStrLn $ "Prover : " ++ prover
        putTables True (only ix tables)

type Res = [Maybe Double]

-- Typical lines
-- SAT AnyMorphism.big_sat_app_any_morphism_fail_step    	p:----	x:----	z:----	v:----	e:----
-- UNS Ack.big_unsat_ack_cf_step                         	p:----	x:----	z:0.02	v:0.14	e:----
tabulate :: String -> ([Res],[Res])
tabulate = partitionEithers . process . words
  where
    process (res:file:p:x:z:v:e:rest)
        | res == "SAT" = Left here  : there
        | res == "UNS" = Right here : there
      where
        here = map proverRes [p,x,z,v,e]
        there = process rest
    process _                         = []

    proverRes (_:':':'F':_) = Nothing
    proverRes (_:':':'-':_) = Nothing
    proverRes (_:':':time)  = Just (read time)
    proverRes xs            = error $ "proverRes: " ++ xs

