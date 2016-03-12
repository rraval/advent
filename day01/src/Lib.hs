module Lib where

count :: String -> Int
count = sum . map go
  where
    go :: Char -> Int
    go '(' = 1
    go ')' = -1
    go _ = 0
