-- Homework 03

module Golf where

import           Data.List (intercalate, transpose)

-- Ex01: Hopscotch

skips :: [a] -> [[a]]
skips l = map (skips' l) $ ixs l

skips' :: [a] -> Int -> [a]
skips' l n = map snd . filter (divBy n . fst) . zip (ixs l) $ l

divBy :: Int -> Int -> Bool
divBy b a = a `mod` b == 0

ixs :: [a] -> [Int]
ixs l = [1..length l]

-- Ex02: Local maxima (element greater than the surroinding ones)

localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:z:_)) = if x < y && y > z then [y] else [] ++ localMaxima xs
localMaxima _              = []

-- Ex03: Histogram
-- Function takes a list of Ints between 0 and 9 (inclusive)
-- Outputs a vertical histogram showing how many of each number
-- were in the input list

histogram :: [Int] -> String
histogram ns = cols ns ++ "\n" ++ rep '=' 10 ++ "\n0123456789\n"

cols :: [Int] -> String
cols l = intercalate "\n" . transpose $ map (extLenTo m . rep '*') ns
  where
    ns = map (countIn l) [0..9]
    m = maximum ns

extLenTo :: Int -> String -> String
extLenTo mx s = rep ' ' (mx - length s) ++ s

rep :: a -> Int -> [a]
rep c n = take n $ repeat c

countIn :: [Int] -> Int -> Int
countIn ns n = length $ filter (==n) ns

