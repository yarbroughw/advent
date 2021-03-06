module Day2 where

import Data.List (sort)
import Data.List.Split (splitOn)

data Box = Box Int Int Int

readBox :: String -> Box
readBox string = Box l w h
  where [l,w,h] = map read $ splitOn "x" string

wrappingFeet :: Box -> Int
wrappingFeet (Box l w h) = surfaceArea + extra
  where sides = [l*w, w*h, h*l]
        surfaceArea = sum (map (*2) sides)
        extra = minimum sides

ribbonFeet :: Box -> Int
ribbonFeet (Box l w h) = wrap + bow
  where wrap = sum $ map (*2) $ take 2 $ sort [l,w,h]
        bow = l*w*h

solution :: String -> IO ()
solution input = do
  let boxes = (map readBox . lines) input
  print (sum $ map wrappingFeet boxes)
  print (sum $ map ribbonFeet boxes)
