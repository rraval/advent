module Lib where

floorOffset :: Char -> Int
floorOffset '(' = 1
floorOffset ')' = -1
floorOffset _ = 0

count :: String -> Int
count = sum . map floorOffset

findPos :: Int -> String -> Maybe Int
findPos target = go 0 0
  where
    go :: Int -> Int -> String -> Maybe Int
    go floor pos _ | floor == target = Just pos
    go floor pos [] = Nothing
    go floor pos (x:xs) = go (floor + floorOffset x) (pos + 1) xs
