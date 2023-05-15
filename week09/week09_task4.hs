import Data.List
main :: IO()
main = do
 print $ findUncles t 5 == [3,4]
 print $ findUncles t 7 == [2,4]
 print $ findUncles t 10 == [5]
    
type Tree = [(Int, [Int])]
type Node = Int
type Graph = [(Node, [Node])]
type Path = [Node]

t :: Tree
t = [(1,[2,3,4]),(2,[5,6]),(3,[7]),(4,[8,9]),(5,[]),(6,[10]),(7,[]),(8,[]),(9,[]),(10,[])]

successors :: Tree -> Int -> [Int]
successors [] _ = error "element not present"
successors ((parent, succ):xs) start
 | start == parent = succ
 | otherwise = successors xs start

isNode :: Node -> Tree -> Bool
isNode node tree = rt node || lf node
 where 
    rt :: Node -> Bool
    rt x = elem x (map fst tree)
    lf :: Node -> Bool
    lf x = elem x (concat (map snd tree))

isLeaf :: Node -> Tree -> Bool
isLeaf node tree = isNode node tree && null (successors tree node)

parent :: Node -> Tree -> Node
parent node [] = 0
parent node tree
 | elem node (successors tree first_node) = first_node
 | otherwise = parent node (tail tree)
 where first_node = fst (head tree)

findUncles :: Tree -> Int -> [Int]
findUncles xs n = filter (\ x -> x/= (parent n xs)) (successors xs (parent (parent n xs) xs))
 
