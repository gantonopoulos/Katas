module DNA (interpretNucleotide, nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, insertWith, empty)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

interpretNucleotide::String -> (Map Nucleotide Int) -> Either String (Map Nucleotide Int)
interpretNucleotide [] map = Right map
interpretNucleotide (element:xs) map
    | element == 'A' = interpretNucleotide xs (insertWith (+) A 1 map)
    | element == 'C' = interpretNucleotide xs (insertWith (+) C 1 map)
    | element == 'G' = interpretNucleotide xs (insertWith (+) G 1 map)
    | element == 'T' = interpretNucleotide xs (insertWith (+) T 1 map)
    | otherwise = Left "Error"


nucleotideCounts :: String -> Either String (Map Nucleotide Int)

nucleotideCounts xs = interpretNucleotide xs empty