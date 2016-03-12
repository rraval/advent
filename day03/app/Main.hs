module Main where

import System.Environment (getArgs)

import Lib

main :: IO ()
main = getArgs >>= readFile . head >>= print . countHouses . deliverPresents . parseDirList
