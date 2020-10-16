-- https://www.seas.upenn.edu/~cis194/spring13/hw/08-IO.pdf
module Party where

import           Data.List (sort)
import           Data.Tree (Tree (rootLabel, subForest))
import           Employee  (Employee (empFun, empName), GuestList (..))

instance Semigroup GuestList where
  (GL l1 f1) <> (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

glCons :: Employee -> GuestList -> GuestList
glCons e (GL [] _)    = GL [e] (empFun e)
glCons e (GL lst fun) = GL (e : lst) (empFun e + fun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 < f2   = gl1
  | otherwise = gl2

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f z node  = f (rootLabel node) foldedChildren
  where
    foldedChildren = map (treeFold f z) (subForest node)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss []         = (singleton boss, mempty)
nextLevel boss guestLists = (withBoss, withoutBoss)
  where
    withBoss = foldr (<>) (singleton boss) (map snd guestLists)
    withoutBoss = mconcat $ map fst guestLists

maxFun :: Tree Employee -> GuestList
maxFun tree =
  let (withBoss, withoutBoss) = treeFold nextLevel (mempty, mempty) tree
  in case compare withBoss withoutBoss of
    GT -> withBoss
    _  -> withoutBoss

singleton :: Employee -> GuestList
singleton e = GL [e] (empFun e)

format :: GuestList -> String
format (GL emps fun) = "Total fun: " ++ show fun ++ "\n" ++ names
  where names = unlines . sort $ map empName emps

main :: IO ()
main = do
  companyTree <- readFile "company.txt"
  (readLn :: IO (Tree Employee)) >>= putStrLn . format . maxFun
