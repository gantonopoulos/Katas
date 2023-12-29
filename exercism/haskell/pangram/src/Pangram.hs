module Pangram (isPangram) where
import Data.Char (toLower, isLetter, isAscii)
import Data.List (nub)


toUnique:: String->String
toUnique text = nub text

uniqueLength:: String->Int
uniqueLength input = length $ nub $ map toLower $ filter (isLetter) $ filter (isAscii) input

isPangram :: String -> Bool
isPangram text = uniqueLength text == 26