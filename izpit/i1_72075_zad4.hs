main :: IO()
main = do
 print $ deepestNodesSum odd t1 == 7
 print $ deepestNodesSum even t2 == 4

data BTree = Empty | Node Int BTree BTree

t1 :: BTree
t1 = Node 1 (Node 2 (Node 4 (Node 7 Empty Empty) Empty) (Node 5 Empty Empty)) (Node 3 Empty (Node 6 Empty (Node 8 Empty Empty)))

t2 :: BTree
t2 = Node 1 (Node 2 (Node 4 Empty Empty) Empty) (Node 3 Empty Empty) 

height :: BTree -> Int
height Empty = 0
height (Node val l r) = 1 + max (height l) (height r)

getLevel :: BTree -> Int -> [Int]
getLevel Empty _ = []
getLevel (Node val _ _) 1 = [val]
getLevel (Node val l r) k = getLevel l (k-1) ++ getLevel r (k-1)

deepestNodesSum :: (Int -> Bool) -> BTree -> Int
deepestNodesSum f t = sum $ filter f (getLevel t (height t))