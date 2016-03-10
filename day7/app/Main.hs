module Main where

import System.Environment (getArgs)
import Text.Trifecta (parseFromFile)

import Data
import Eval
import Parser

main :: IO ()
main = getArgs >>= go
  where
    go (file:wire:_) = runProgram file wire >>= print
    go _ = putStrLn "Usage: day7 FILE WIRE"

runProgram :: String -> String -> IO (Maybe LookupResult)
runProgram file wire = do
    insts <- parseFromFile instructionListParser file
    return $ (flip eval) (WireName wire) <$> insts
