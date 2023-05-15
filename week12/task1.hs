main :: IO()
main = do
    print $ rangedSum firstTree 100 50 == 0 
    print $ rangedSum firstTree 7 15 == 32 
    print $ rangedSum firstTree 15 7 == 32 
    print $ rangedSum secondTree 6 10 == 23 
    print $ rangedSum secondTree 10 6 == 23 

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

firstTree :: BTree Int
firstTree = Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 Nil (Node 18 Nil Nil))

secondTree :: BTree Int
secondTree = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) (Node 7 (Node 6 Nil Nil) Nil)) Nil) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

rangedSum :: (Num a, Ord a) => BTree a -> a -> a -> a
rangedSum Nil _ _ = 0
rangedSum tree@(Node value left right) l r
 | l >= r = rangedSum tree r l
 | value >= l && value <= r = value + (rangedSum left l r) + (rangedSum right l r)
 | otherwise = (rangedSum left l r) + (rangedSum right l r)