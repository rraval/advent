module Main where

import Control.Applicative (liftA2)
import Data.List (foldl1')
import System.Environment (getArgs)

import Lib

main :: IO ()
main = getArgs >>= go
  where
    go :: [String] -> IO ()
    go ("first":file:_) = go' file wrapping
    go ("second":file:_) = go' file ribbon
    go _ = putStrLn "Usage: day2-exe (first FILE | second FILE)"

    go' :: String -> (Present -> Int) -> IO ()
    go' file func = readFile file >>= print . sumMaybe . map (func <$>) . parsePresentList

    parsePresentList :: String -> [Maybe Present]
    parsePresentList = map parsePresent . lines

    sumMaybe :: [Maybe Int] -> Maybe Int
    sumMaybe = foldl1' $ liftA2 (+)
