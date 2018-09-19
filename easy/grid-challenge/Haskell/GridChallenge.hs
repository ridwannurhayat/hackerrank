module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the gridChallenge function below.
gridChallenge [] = "YES"
gridChallenge (x:xs)
    | xs == [] = "YES"
    | not (compareStr (sort x) (sort (head xs))) = "NO"
    | otherwise = gridChallenge xs

compareStr [] [] = True
compareStr (x:xs) (y:ys)
    | x <= y = compareStr xs ys
    | otherwise = False

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    t <- readLn :: IO Int

    forM_ [1..t] $ \t_itr -> do
        n <- readLn :: IO Int

        grid <- readMultipleLinesAsStringArray n

        let result = gridChallenge grid

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr
