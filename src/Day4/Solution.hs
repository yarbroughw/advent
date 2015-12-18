module Day4.Solution where

import Data.ByteString.Char8 (pack, unpack)
import Data.Hash.MD5

key :: String -> Int -> String
key input int = input ++ (show int)

hash :: String -> String
hash = md5s . Str

hashWithZeroes :: Int -> String -> Int
hashWithZeroes numZeroes input = head $ filter (hasNumZeroes . hash . (key input)) [1..]
  where hasNumZeroes = all (== '0') . take numZeroes

day4 :: IO ()
day4 = do
  let input = "bgvyzdsv"
  print $ hashWithZeroes 5 input
  print $ hashWithZeroes 6 input
