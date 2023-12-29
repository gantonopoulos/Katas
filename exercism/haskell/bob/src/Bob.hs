module Bob (responseFor) where
import Data.List (isSuffixOf, dropWhileEnd)
import Data.Char (isUpper, isSpace, isAlpha)

responseFor :: String -> String
responseFor xs
    | "?" `isSuffixOf` endTrimmed
        && isShouting = "Calm down, I know what I'm doing!"
    | "?" `isSuffixOf` endTrimmed = "Sure."
    | isShouting = "Whoa, chill out!"
    | all isSpace xs = "Fine. Be that way!"
    | otherwise = "Whatever."
    where isShouting = all isUpper onlyLetters && (not . null) onlyLetters 
          onlyLetters = filter isAlpha xs
          endTrimmed = dropWhileEnd isSpace xs