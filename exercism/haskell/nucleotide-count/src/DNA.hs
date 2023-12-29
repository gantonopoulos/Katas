module DNA (
    read', 
    nucleotideCounts, 
    count,
    parse,
    Nucleotide(..)) where

import Data.Map (Map, insertWith, empty)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

count::[Nucleotide]->Map Nucleotide Int
count [] = empty
count nucleotides = foldr(\element acc->insertWith (+) element 1 acc) empty nucleotides

read'::Char->Either String Nucleotide
read' element 
    | element == 'A' = Right A
    | element == 'C' = Right C
    | element == 'G' = Right G
    | element == 'T' = Right T
    | otherwise = Left "Error"


parse::String -> Either String [Nucleotide]
parse = traverse read'

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts elements =  fmap count $ parse elements