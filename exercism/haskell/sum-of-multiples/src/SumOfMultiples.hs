module SumOfMultiples (
    sumOfMultiples,
    multiplesOfFactor) where

import qualified Data.Set as Set
import Data.Set (fromList, toList, union)

multiplesOfFactor :: Integer -> Integer ->[Integer]
multiplesOfFactor factor limit 
    | factor < 0  = []
    | limit <= factor = []
    | otherwise = factor : fmap (+factor) (multiplesOfFactor factor (limit-factor))

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = 0
