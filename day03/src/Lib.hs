{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where

import Control.Applicative (liftA2)
import Data.List (foldl', foldl1', transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M

data Direction = DirLeft | DirUp | DirRight | DirDown deriving (Eq, Show)

parseDir :: Char -> Maybe Direction
parseDir '<' = Just DirLeft
parseDir '^' = Just DirUp
parseDir '>' = Just DirRight
parseDir 'v' = Just DirDown
parseDir _ = Nothing

parseDirList :: String -> [Direction]
parseDirList = catMaybes . map parseDir

type X  = Int
type Y = Int
newtype PresentCount = PresentCount Int deriving (Eq, Show, Ord, Num)

data House = House X Y deriving (Eq, Show, Ord)
type HouseMap = M.Map House PresentCount

data DeliveryState = DeliveryState
    { currentHouse :: House
    , houseMap :: HouseMap
    }
  deriving (Eq, Show)

deliverPresents :: [[Direction]] -> [DeliveryState]
deliverPresents = map $ foldl' go initial
  where
    initial :: DeliveryState
    initial = DeliveryState origin $ M.singleton origin $ PresentCount 1

    origin :: House
    origin = House 0 0

    go :: DeliveryState -> Direction -> DeliveryState
    go state dir = let next = lookupHouse dir $ currentHouse state in
        DeliveryState
            { currentHouse = next
            , houseMap = M.insertWith (+) next (PresentCount 1) $ houseMap state
            }

    lookupHouse :: Direction -> House -> House
    lookupHouse DirUp (House x y) = House x $ y + 1
    lookupHouse DirDown (House x y) = House x $ y - 1
    lookupHouse DirLeft (House x y) = House (x - 1) y
    lookupHouse DirRight (House x y) = House (x + 1) y

countHouses :: [DeliveryState] -> Int
countHouses = M.size . foldl1' merge . map houseMap
  where
    merge :: HouseMap -> HouseMap -> HouseMap
    merge = M.unionWith (+)

-- list of n lists, which the i'th list is composed of element positions
-- j % n == i of the original list
streams :: Int -> [a] -> [[a]]
streams n = transpose . chunksOf n
