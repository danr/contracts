{-# LANGUAGE ParallelListComp,ViewPatterns #-}
module ResultSummary where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import Data.List.Split
import System.Environment
import Text.Printf

main = do
    files <- getArgs
    mapM_ printSummary files

printSummary file = do
    header:results <- map uncsv . lines <$> readFile file
    putStrLn $ file ++ ", files: " ++ show (length results)
    putStrLn $ unlinePad
        [ [tool,"avg: " ++ showTime (avg times),"timeouts: " ++ show (timeouts times)]
        | tool <- tail header
        | times_raw <- tail (transpose results)
        , let times = map parseTime times_raw
        ]

unlinePad :: [[String]] -> String
unlinePad xs = unlines [ concat $ zipWith pad row lengths | row <- xs ]
  where
    lengths = map (maximum . map length) (transpose xs)
    pad s i = s ++ replicate (i - length s  + 1) ' '

showTime :: Double -> String
showTime = printf "%.1fms"

uncsv :: String -> [String]
uncsv = unintercalate ","

timeouts :: [Maybe Integer] -> Int
timeouts = length . filter isNothing

avg :: [Maybe Integer] -> Double
avg (mapMaybe (fmap fromInteger) -> xs) = sum xs / genericLength xs

-- Remove the last "ms" from the string
parseTime :: String -> Maybe Integer
parseTime "---" = Nothing
parseTime xs = Just . read . reverse . drop 2 . reverse $ xs
