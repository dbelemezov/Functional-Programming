main :: IO()
main = do
    print $ levelSum numberBTree 1 == 11
    print $ cone numberBTree == True

data BTree a = Nil | Node a (BTree a) (BTree a)

numberBTree :: BTree Int
numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))


levelSum :: BTree Int -> Int -> Int
levelSum (Node value left right) k = sum $ helper (Node value left right) k
    where
        helper :: BTree Int -> Int -> [Int]
        helper Nil _ = []
        helper (Node value left right) 0 = [value]
        helper (Node value left right) k = helper left (k-1) ++ helper right (k-1)

cone :: BTree Int -> Bool
cone Nil = True
cone tree = helper tree 0
    where 
        helper :: BTree Int -> Int -> Bool
        helper (Node value left right) k
         | levelSum  (Node value left right) (k+1) == 0 = True
         | (levelSum (Node value left right) k ) > (levelSum (Node value left right) (k+1)) = False
         | otherwise = helper left (k+1) && helper right (k+1)
        

{-
getLevel :: BTree Int-> Int -> [Int]
getLevel Nil _ = []
getLevel (Node v _  _)  0 = [v]
getLevel (Node v lt rt) k = getLevel lt (k - 1) ++ getLevel rt (k - 1)

levelSum :: BTree Int-> Int -> Int
levelSum bt k = sum (getLevel bt k)        

height :: BTree Int-> Int
height Nil = 0
height (Node v lt rt) = 1 + max (height lt) (height rt)

cone :: BTree Int-> Bool
cone bt = helper bt (height bt - 1) 0
    where
        helper bt h i
         | i == h = True
         | levelSum bt i < levelSum bt (i + 1) = helper bt h (i + 1)
         | otherwise = False

-}         