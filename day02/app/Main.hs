module Main where

import Control.Applicative (liftA2)
import Data.List (foldl1')
import System.Environment (getArgs)

import Lib

main :: IO ()
main = getArgs >>= readFile . head >>= print . sumMaybe . toWrapping . parsePresentList
  where
    parsePresentList :: String -> [Maybe Present]
    parsePresentList = map parsePresent . lines

    toWrapping :: [Maybe Present] -> [Maybe Int]
    toWrapping = map (wrapping <$>)

    sumMaybe :: [Maybe Int] -> Maybe Int
    sumMaybe = foldl1' $ liftA2 (+)
