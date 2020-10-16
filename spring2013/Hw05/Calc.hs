{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- https://www.seas.upenn.edu/~cis194/spring13/hw/05-type-classes.pdf

module Calc where

import qualified ExprT   as E
import           Parser
import           StackVM (Program, StackExp (..))

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n
    | n <= 0    = False
    | otherwise = True

  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

instance Expr Program where
  lit n = [PushI n]
  add pa pb = pa ++ pb ++ [Add]
  mul pa pb = pa ++ pb ++ [Mul]


eval :: E.ExprT -> Integer
eval (E.Lit n)   = n
eval (E.Add a b) = eval a + eval b
eval (E.Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp E.Lit E.Add E.Mul s
