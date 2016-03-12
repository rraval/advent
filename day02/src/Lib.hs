module Lib where

import Data.List.Split (splitOn)

newtype Length = Length Int deriving (Eq, Show)
newtype Width = Width Int deriving (Eq, Show)
newtype Height = Height Int deriving (Eq, Show)

data Present = Cuboid Length Width Height deriving (Eq, Show)

parsePresent :: String -> Maybe Present
parsePresent = go . map read . splitOn "x"
  where
    go :: [Int] -> Maybe Present
    go (l:w:h:_) = Just $ Cuboid (Length l) (Width w) (Height h)
    go _ = Nothing

wrapping :: Present -> Int
wrapping (Cuboid (Length l) (Width w) (Height h)) = 2 * sum faces + minimum faces
  where
    f1 = l * w
    f2 = w * h
    f3 = h * l
    faces = [f1, f2, f3]
