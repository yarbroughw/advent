module Day1.Solution where

import Data.List (findIndex, inits)

-- slower than a foldr (does 2 passes over input), but more readable/declarative
floorCount :: String -> Int
floorCount input = ups - downs
  where ups   = charCount '(' input
        downs = charCount ')' input
        charCount c = length . filter (== c)


floorCount2 :: String -> Int
floorCount2 = foldr ((+) . parenvals) 0
  where parenvals '(' = 1
        parenvals ')' = (-1)
        parenvals _ = 0


hitBasement :: String -> Maybe Int
hitBasement input = findIndex (< 0) trip
  where trip = map floorCount $ inits input


day1 :: IO ()
day1 = do
  input <- readFile "input.txt"
  print $ floorCount input  -- part 1
  print $ hitBasement input -- part 2
