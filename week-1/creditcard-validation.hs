toDigits:: Integer -> [Integer]
toDigits number
  | number <= 0 = []
  | otherwise  = toDigits(number `div` 10) ++ [number `mod` 10]

toDigitsRev:: Integer -> [Integer]
toDigitsRev number
  | number <= 0 = []
  | otherwise  =  number `mod` 10 : toDigitsRev(number `div` 10)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther(xs)
  | length(xs) `mod` 2 == 0 = zipWith ($) (cycle([(*2),(*1)])) xs
  | otherwise = zipWith ($) (cycle([(*1),(*2)])) xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
  | x > 10 = sumDigits(toDigits(x))
  | otherwise = x
sumDigits (x:xs) = sumDigits(toDigits(x)) + sumDigits(xs)

validate :: Integer -> Bool
validate x
  | sumOfAllDigits `mod` 10 == 0 = True
  | otherwise = False
  where sumOfAllDigits = sumDigits $ doubleEveryOther $ toDigits x
