main :: IO()
main = do
 print $ isGraceful t1 == True
 print $ isGraceful t2 == False
    
data NTree a = Nil | Node a [NTree a]
 deriving (Show, Eq)

t1 ::  (Num a) => NTree a
t1 = Node 1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]

t2 ::  (Num a) => NTree a 
t2 = Node 7 [Node 9 [Node 5 [Nil], Node 2 [Nil]]]

isGraceful :: NTree Int -> Bool
isGraceful Nil = True
isGraceful (Node x xs) = and ((map check xs) ++ map isGraceful xs)
    where
        check :: NTree Int -> Bool
        check Nil = True
        check (Node y ys) = even (abs (x - y))

