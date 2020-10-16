-- https://www.seas.upenn.edu/~cis194/spring13/hw/06-laziness.pdf

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show s = show . take 20 $ show' s
    where show' (Cons x rest) = x : show' rest

streamToList :: Stream a -> [a]
streamToList (Cons a rest) = a : streamToList rest

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x rest) = Cons (f x) (streamMap f rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f start = Cons start (streamFromSeed f (f start))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = getRuler $ map streamRepeat [0..]
  where
    getRuler (s:rest) = interleaveStreams s (getRuler rest)

-- ruler :: Stream Integer
-- ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)
