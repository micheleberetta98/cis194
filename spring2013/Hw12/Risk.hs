{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import           Control.Monad.Random
import           Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice :: Army -> Rand StdGen [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data WinningArmy = Attack | Defense
  deriving (Eq)

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

invade ::Battlefield -> Rand StdGen Battlefield
invade bf
  | attackers bf < 2 || defenders bf == 0 = return bf
  | otherwise                             = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM 1000 (invade bf) >>= probAttackWins

probAttackWins :: [Battlefield] -> Rand StdGen Double
probAttackWins bfs = return (attackStands bfs / len bfs)
  where
    len = fromIntegral . length
    attackStands = fromIntegral . count ((== 0) . defenders)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  attack <- dice att
  defense <- dice def
  return (outcome bf attack defense)
  where
    att = usableUnits 3 (attackers bf)
    def = usableUnits 2 (defenders bf)

outcome :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
outcome bf a d = Battlefield remAttack remDefense
  where
    (aDeaths, dDeaths) = deaths a d
    remAttack = attackers bf - aDeaths
    remDefense = defenders bf - dDeaths

deaths:: [DieValue] -> [DieValue] -> (Army, Army)
deaths attack defense = (aDeaths, dDeaths)
  where
    winningRolls = map winner (sortedDice attack defense)
    aDeaths = count (== Defense) winningRolls
    dDeaths = count (== Attack) winningRolls

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

sortedDice :: [DieValue] -> [DieValue] -> [(DieValue, DieValue)]
sortedDice att def = zipWith (,) (sorted att) (sorted def)
  where sorted = reverse . sort

winner :: (DieValue, DieValue) -> WinningArmy
winner (a, d)
  | a > d     = Attack
  | otherwise = Defense

usableUnits :: Army -> Army -> Army
usableUnits _ 0 = 0
usableUnits maxUnits totalUnits
  | totalUnits < maxUnits = totalUnits - 1
  | otherwise             = maxUnits
