module Main where

import Paths_advent
import Control.Monad ((<=<))

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5

data Solution = Solution {
  name         :: String,
  solution     :: String -> IO (),
  dataFileName :: String
}

getData :: String -> IO String
getData = readFile <=< getDataFileName

runSolution :: Solution -> IO ()
runSolution s = do
  putStrLn "==============================================="
  putStrLn $ "Calculating solution(s) for " ++ name s ++ "... "
  input <- getData (dataFileName s)
  solution s input

solutions :: [Solution]
solutions =
  [ Solution "Day 1" Day1.solution "day1input.txt"
  , Solution "Day 2" Day2.solution "day2input.txt"
  , Solution "Day 3" Day3.solution "day3input.txt"
  , Solution "Day 4" Day4.solution "day4input.txt"
  , Solution "Day 5" Day5.solution "day5input.txt"
  ]

main :: IO ()
main = mapM_ runSolution solutions
