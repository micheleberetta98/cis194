module Hw01Hanoi (hanoi) where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 start finish tmp = [(start, finish)]
hanoi nDisks start finish tmp
  | nDisks <= 0 = []
  | otherwise   = moveUpperTowerToTmp ++ [(start, finish)] ++ moveUpperTowerToFinish
      where
        moveUpperTowerToTmp = hanoi (nDisks - 1) start tmp finish
        moveUpperTowerToFinish = hanoi (nDisks - 1) tmp finish start
