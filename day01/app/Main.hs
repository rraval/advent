module Main where

import System.Environment (getArgs)

import Lib (count)

main :: IO ()
main = getArgs >>= readFile . head >>= print . count
