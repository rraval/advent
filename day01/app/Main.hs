module Main where

import System.Environment (getArgs)

import Lib (count, findPos)

main :: IO ()
main = getArgs >>= go
  where
    go :: [String] -> IO ()
    go ("first":file:_) = readFile file >>= print . count
    go ("second":file:target:_) = readFile file >>= print . findPos (read target)
    go _ = putStrLn "Usage: day1-exe (first INPUT | second INPUT TARGET)"
