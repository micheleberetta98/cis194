module Hw01CreditCard (validate) where
-- https://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEverySecond . reverse
  where
    doubleEverySecond []       = []
    doubleEverySecond (x:[])   = [x]
    doubleEverySecond (x:y:xs) = x : 2*y : doubleEverySecond xs

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (n:ns) = sum (toDigits n) + sumDigits ns

validate :: Integer -> Bool
validate n =
  let s = sumDigits . doubleEveryOther . toDigits $ n
  in s `mod` 10 == 0
