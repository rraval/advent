module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BSC

import Lib

main :: IO ()
main = getArgs >>= go
  where
    go :: [String] -> IO ()
    go (prefix:threshold:_) = print $ mine (BSC.pack prefix) (read threshold)
    go _ = putStrLn "Usage: day4-exe PREFIX THRESHOLD"
