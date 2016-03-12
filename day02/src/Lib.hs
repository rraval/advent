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

ribbon :: Present -> Int
ribbon (Cuboid (Length l) (Width w) (Height h)) = minimum [p1, p2, p3] + vol
  where
    p1 = 2*l + 2*w
    p2 = 2*w + 2*h
    p3 = 2*h + 2*l
    vol = l * h * w
