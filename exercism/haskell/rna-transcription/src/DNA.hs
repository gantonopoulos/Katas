module DNA (toRNA) where

decideComplement :: String -> String -> Either Char String
decideComplement [] tail = Right tail
decideComplement (x:xs) tail
    | x == 'G' =  decideComplement xs (tail++"C")
    | x == 'C' =  decideComplement xs (tail++"G")
    | x == 'T' =  decideComplement xs (tail++"A")
    | x == 'A' =  decideComplement xs (tail++"U")
    | otherwise = Left x

toRNA :: String -> Either Char String
toRNA xs = decideComplement xs ""
