-- https://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf

module LogAnalysis (parse, inOrder, build, whatWentWrong) where

import           Data.List (intercalate)
import           Log       (LogMessage (..), MessageTree (..), MessageType (..),
                            TimeStamp)

-------------- ANALYSIS --------------

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getErrorMessage . filter (isErrorGreaterThan 50)
  where
    isErrorGreaterThan n ((LogMessage (Error s) _ _)) = s > n
    isErrorGreaterThan _ _                            = False

    getErrorMessage (LogMessage _ _ msg) = msg

-------------- LOG PARSING --------------

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage = parseLine . words

parseLine :: [String] -> LogMessage
parseLine ("I":t:rest)   = parseLogMessage Info t rest
parseLine ("W":t:rest)   = parseLogMessage Warning t rest
parseLine ("E":s:t:rest) = parseLogMessage (Error (read s)) t rest
parseLine msg            = Unknown (join msg)

parseLogMessage :: MessageType -> String -> [String] -> LogMessage
parseLogMessage tp t msg = LogMessage tp (read t) (join msg)

join :: [String] -> String
join = intercalate " "

-------------- TREE MANAGEMENT --------------

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                   = []
inOrder (Node left root right) = inOrder left ++ [root] ++ inOrder right

build :: [LogMessage] -> MessageTree
build []     = Leaf
build (m:ms) = insert m (build ms)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree           = tree
insert msg Leaf                   = Node Leaf msg Leaf
insert msg (Node left root right)
  | timestamp msg < timestamp root = Node (insert msg left) root right
  | otherwise                      = Node left root (insert msg right)

timestamp :: LogMessage -> TimeStamp
timestamp (Unknown _)        = 0
timestamp (LogMessage _ t _) = t

