main :: IO()
main = do
 print $ height numberBTree == 4
 print $ height charBTree == 3
 print $ average numberBTree == 16.22
 --print $ average charBTree -- should not work
 print $ sumLeaves numberBTree == 119
 --print $ sumLeaves charBTree -- shouldn't work
 print $ areEqual charBTree charBTree == True
 print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))) == False
 print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil Nil))) == False
 print $ mirrorTree numberBTree == Node 5 (Node 4 (Node 5 (Node 21 Nil Nil) Nil) (Node 2 Nil Nil)) (Node 12 (Node 0 Nil Nil) (Node 1 Nil (Node 96 Nil Nil)))
 print $ mirrorTree charBTree == Node 'k' (Node 'l' (Node 'l' Nil Nil) (Node 'e' Nil Nil)) (Node 'a' (Node 's' Nil Nil) (Node 'h' Nil Nil))

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

numberBTree :: (Num a) => BTree a
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))

charBTree :: BTree Char
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil)) 

sumTree :: (Num a) => BTree a -> a
sumTree Nil = 0
sumTree (Node value lt rt) = value + sumTree lt + sumTree rt

size :: BTree a -> Int
size Nil = 0
size (Node _ lt rt) = 1 + size lt + size rt

paths :: BTree a -> [[a]] 
paths Nil = [[]]
paths (Node x lt rt) = map (x:) (paths lt ++ paths rt)

height :: BTree a -> Int
height Nil = 0
height (Node _ lt rt) = 1 + max (height lt) (height rt)

average ::(Fractional a, Num a,RealFrac a) => BTree a -> a
average Nil = 0
average (Node x lt rt) =  fromInteger (round ((sumTree (Node x lt rt)) / fromIntegral (size (Node x lt rt)) *100)) / 100         

sumLeaves ::(Fractional a, Num a,RealFrac a) => BTree a -> a
sumLeaves Nil = 0
sumLeaves (Node v Nil Nil) = v
sumLeaves (Node _ lt rt) = sumLeaves lt + sumLeaves rt

areEqual ::(Eq a)=> BTree a -> BTree a -> Bool
areEqual Nil Nil = False
areEqual (Node x lt rt) Nil = False
areEqual Nil (Node y lt rt) = False
areEqual (Node x lt rt) (Node y lt1 rt1) = paths (Node x lt rt) == paths (Node y lt1 rt1)

mirrorTree :: BTree a -> BTree a
mirrorTree Nil = Nil
mirrorTree (Node x lt rt) = Node x (mirrorTree rt) (mirrorTree lt)
