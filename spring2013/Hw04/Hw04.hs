-- https://www.seas.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf

module Hw04 where

-- Ex01: reimplement fun1 and fun2 into fun1' and fun2'

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map minusTwo . filter even
  where minusTwo x = x - 2

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

