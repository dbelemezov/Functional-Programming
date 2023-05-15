main :: IO()
main = do
    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

tree :: BTree Int
tree = Node 4 (Node 1 (Node 0 Nil Nil)(Node 2 Nil (Node 3 Nil Nil)))(Node 6 (Node 5 Nil Nil)(Node 7 Nil (Node 8 Nil Nil)))

{-
nodes :: BTree a -> [a]
nodes Nil = []
nodes (Node value left right) = (value : nodes left) ++ nodes right

convert :: BTree Int -> BTree Int
convert Nil = Nil
convert tree = helper tree
 where
     helper :: BTree Int -> BTree Int
     helper Nil = Nil
     helper (Node value left right) = Node (sum $ filter (>= value) (nodes tree)) (helper left) (helper right)
-}

nodes :: BTree a-> [a]
nodes  Nil = []
nodes (Node value left right) = [value] ++ nodes left ++ nodes right

sumNodes :: BTree Int-> Int -> Int
sumNodes tree value = sum[ x | x <- (nodes tree) , x >= value]

convert :: BTree Int -> BTree Int
convert Nil = Nil
convert tree = create tree tree
  where
      create :: BTree Int-> BTree Int-> BTree Int
      create tree Nil = Nil
      create tree (Node value left right) = (Node (sumNodes tree value) (create tree left) (create tree right) )
        

