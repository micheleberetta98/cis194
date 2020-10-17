module Ex1 where

import           Data.Char
import           Data.Function
import           Data.List

halveEvens :: [Integer] -> [Integer]
halveEvens = map (`div` 2) . filter even

safeString :: String -> String
safeString = map replace
  where
    replace c
      | isControl c = '_'
      | isAscii c = c
      | otherwise = '_'

holes :: [a] -> [[a]]
holes xs = zipWith (++) (inits xs) (tails' xs)
  where tails' xs = map tail (init (tails xs))

longestText :: Show a => [a] -> a
longestText = maximumBy (compare `on` (length . show))

adjacents :: [a] -> [(a,a)]
adjacents xs = zip xs (tail xs)

commas :: [String] -> String
commas = intercalate ", "

addPolynomials :: [[Integer]] -> [Integer]
addPolynomials = foldl1' (zipWith (+))

sumNumbers :: String -> Integer
sumNumbers = sum . map read . filter areDigits . groupBy ((==) `on` isDigit)
  where areDigits = isDigit . head
