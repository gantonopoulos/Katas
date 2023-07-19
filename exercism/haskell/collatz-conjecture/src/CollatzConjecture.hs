    module CollatzConjecture (collatz) where

    computeCollatz:: Integer -> Integer -> Integer
    computeCollatz 1 steps = steps
    computeCollatz n steps
        | odd n = computeCollatz (3*n+1) (steps+1)
        | otherwise = computeCollatz (n `div` 2) (steps+1)    

    collatz :: Integer -> Maybe Integer
    collatz n
        | n < 0 = Nothing    
        | n == 0 = Nothing    
        | otherwise = Just (computeCollatz n 0)