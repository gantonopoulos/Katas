module LeapYear (isLeapYear) where

isEvenlyDivisibleBy :: Integer -> Integer -> Bool
isEvenlyDivisibleBy divider = (==0) . (`mod` divider)

isEvenlyDivisibleBy4 :: Integer -> Bool
isEvenlyDivisibleBy4 = isEvenlyDivisibleBy 4

isEvenlyDivisibleBy100 :: Integer -> Bool
isEvenlyDivisibleBy100 = isEvenlyDivisibleBy 100

isEvenlyDivisibleBy400 :: Integer -> Bool
isEvenlyDivisibleBy400 = isEvenlyDivisibleBy 400

isLeapYear :: Integer -> Bool
isLeapYear year = isEvenlyDivisibleBy4 year && (not(isEvenlyDivisibleBy100 year) || isEvenlyDivisibleBy400 year)
