main :: IO()
main = do
 print $ minDepthGreenNode colorTree == 3

data Color = Red | Green | Blue
 deriving (Show, Eq)

data Tree = Empty | Node Color Tree Tree
 deriving (Show, Eq) 

colorTree :: Tree 
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)

paths :: Tree  -> [[Color]] 
paths Empty = [[]]
paths (Node x lt rt) = map (x:) (paths lt ++ paths rt)

getAllGreenPaths :: Tree -> [[Color]] 
getAllGreenPaths (Node x lt rt) = filter (\ x -> last x == Green) (paths(Node x lt rt))

minDepthGreenNode :: Tree -> Int
minDepthGreenNode (Node x lt rt) = minimum $ map (\ x -> length x) (getAllGreenPaths(Node x lt rt))
