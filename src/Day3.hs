module Day3 where

import Data.Set (fromList, union)

type House = (Int, Int)
data Direction = North | East | South | West

readDirection :: Char -> Direction
readDirection '^' = North
readDirection '>' = East
readDirection 'v' = South
readDirection '<' = West

travelFrom :: House -> Direction -> House
travelFrom (x,y) d = case d of
  North -> (x,y+1)
  East  -> (x+1,y)
  South -> (x,y-1)
  West  -> (x-1,y)

travel :: [Direction] -> [House]
travel directions = scanl travelFrom (0,0) directions

travelBy :: (Int -> Bool) -> [Direction] -> [House]
travelBy pred directions = travel $ filtered directions
  where filtered = map snd . filter (pred . fst) . zip [1..]

numDistinct :: (Ord a) => [a] -> Int
numDistinct = length . fromList

part1 :: [Direction] -> Int
part1 directions = numDistinct houses
    where houses = travel directions

part2 :: [Direction] -> Int
part2 directions = numDistinct houses
    where houses = santaHouses ++ robotHouses
          santaHouses = travelBy even directions
          robotHouses = travelBy odd directions

solution :: IO ()
solution = do
  directions <- (map readDirection) <$> readFile "input.txt"
  print $ part1 directions
  print $ part2 directions
