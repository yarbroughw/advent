module Day4 where

import Data.ByteString.Char8 (pack, unpack)
import Data.Hash.MD5

key :: String -> Int -> String
key input int = input ++ show int

hash :: String -> String
hash = md5s . Str

hashWithZeroes :: Int -> String -> Int
hashWithZeroes numZeroes input = head $ filter (hasNumZeroes . hash . key input) [1..]
  where hasNumZeroes = all (== '0') . take numZeroes

solution :: String -> IO ()
solution input = do
  print $ hashWithZeroes 5 input
  print $ hashWithZeroes 6 input
