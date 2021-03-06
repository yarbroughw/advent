module Day5 where

import Data.List (group, isInfixOf, tails)

threeVowels :: String -> Bool
threeVowels = (>= 3) . length . filter (`elem` "aeiouAEIOU")

doubleLetter :: String -> Bool
doubleLetter = any ((>1) . length) . group

noBadStrings :: String -> Bool
noBadStrings input = all (not . flip isInfixOf input) badstrings
  where badstrings = ["ab", "cd", "pq", "xy"]

hasPalindromeTriplet :: String -> Bool
hasPalindromeTriplet = any isPalindrome . triplets
  where isPalindrome = (==) <*> reverse
        triplets input = take (length input - 3 + 1) $ map (take 3) $ tails input

repeatedPair :: String -> Bool
repeatedPair = any repeatedPrefixPair . takeWhile ((>=4) . length) . tails
  where repeatedPrefixPair input = take 2 input `isInfixOf` drop 2 input

applyRules :: [String -> Bool] -> String -> Bool
applyRules rules string = and $ sequence rules string

isNice :: String -> Bool
isNice = applyRules [threeVowels, doubleLetter, noBadStrings]

isNicer :: String -> Bool
isNicer = applyRules [hasPalindromeTriplet, repeatedPair]

solution :: String -> IO ()
solution input = do
  print $ length $ filter isNice (lines input)
  print $ length $ filter isNicer (lines input)
