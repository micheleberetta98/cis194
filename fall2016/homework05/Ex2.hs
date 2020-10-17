module Ex2 where

import           Data.List
import           Ex1


wordCount :: String -> String
wordCount s = unlines
  [ "Number of lines: " ++ show (length ls) ++ "\n"
  , "Number of empty lines: " ++ show (length els) ++ "\n"
  , "Number of words: " ++ show (length $ words s) ++ "\n"
  , "Number of unique words: " ++ show (length uniqueWords) ++ "\n"
  , "Number of words followed by themselves" ++ show (wordsFollowedByThemselves s) ++ "\n"
  , "Length of the longest line: " ++ show (longestLineLength s)
  ]
  where
    ls = lines s
    els = filter null ls
    uniqueWords = nub (words s)

wordsFollowedByThemselves :: String -> Int
wordsFollowedByThemselves = length . filter areEqals . adjacents . words
  where areEqals = uncurry (==)

longestLineLength :: String -> Int
longestLineLength = length . longestText . lines
