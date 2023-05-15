import Data.List
main :: IO()
main = do
 print $ ordered t1 == True
 print $ ordered t2 == False

data Tree = Empty | Node (Int,Int) Tree Tree 
 deriving (Show, Eq)

t1 :: Tree 
t1 = Node (3,10) (Node (5,8) (Node (6,7) Empty Empty) (Node (4,9) Empty Empty)) (Node (2,12) Empty (Node (1,15) Empty Empty))
t2 :: Tree 
t2 = Node (3,10) (Node (5,8) (Node (6,7) Empty Empty) (Node (7,9) Empty Empty)) (Node (2,12) Empty (Node (1,15) Empty Empty))

traverseDFS :: Tree -> [Int]
traverseDFS Empty = []
traverseDFS (Node (x,y) lt rt) = (traverseDFS lt) ++ [(y-x)] ++ (traverseDFS rt)

ordered :: Tree -> Bool
ordered t = nodesDFS == sort nodesDFS
 where
     nodesDFS = traverseDFS t
