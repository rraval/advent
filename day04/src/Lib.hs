module Lib where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base16 as BS16

combine :: BS.ByteString -> Int -> BS.ByteString
combine prefix num = BS.append prefix $ BSC.pack $ show num

hash16 :: BS.ByteString -> BS.ByteString
hash16 = BS16.encode . MD5.hash

countLeading :: (Char -> Bool) -> BS.ByteString -> Int
countLeading f = BS.length . BSC.takeWhile f

mine :: BS.ByteString -> Int -> Int
mine prefix threshold = fst $ head $ filter pred sequence
  where
    sequence = [(i, hash16 $ combine prefix i) | i <- [1..]]
    pred (_, h) = countLeading (== '0') h >= threshold
