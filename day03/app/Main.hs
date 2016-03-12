module Main where

import System.Environment (getArgs)

import Lib

main :: IO ()
main = getArgs >>= go
  where
    go :: [String] -> IO ()
    go (num:file:_) = readFile file >>= print . countHouses . deliverPresents . streams (read num) . parseDirList
    go _ = putStrLn "Usage: day3-exe NUM FILE"
