
threeVowels :: String -> Bool
threeVowels = null . filter (`elem` "aeiouAEIOU")

doubleLetter :: String -> Bool
doubleLetter = undefined

noBadStrings :: String -> Bool
noBadStrings = undefined

isNice :: String -> Bool
isNice string = and $ map ($ string) rules
  where rules = [threeVowels, doubleLetter, noBadStrings]
