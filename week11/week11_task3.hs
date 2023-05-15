main :: IO()
main = do
 print $ maxDepthBlueNode colorTree == 3

data Color = Red | Green | Blue
 deriving (Show, Eq)

data Tree = Empty | Node Color Tree Tree
 deriving (Show, Eq) 

colorTree :: Tree 
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)

maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode Empty = -1
maxDepthBlueNode (Node Blue lt rt) = max 0 (1 + max (maxDepthBlueNode lt) (maxDepthBlueNode rt))
maxDepthBlueNode (Node _ lt rt) = 1 + max (maxDepthBlueNode lt) (maxDepthBlueNode rt)
