import Data.List
main :: IO()
main = do
 print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 == [[1]]
 print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 == [[1, 2, 3], [1, 2, 4]]
 print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1, 2], [1, 3]]
 print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2 == [[2,3],[2,4]]
 print $ subseq [1,2,3,4]

type Node = Int
type Graph = [(Node, [Node])]
type Path = [Node]

nodes :: Graph -> [Int]
nodes graph = [ start | (start, _) <- graph]

successors :: Graph -> Int -> [Int]
successors [] _ = error "element not present"
successors ((parent, succ):xs) start
 | start == parent = succ
 | otherwise = successors xs start

isPath :: Graph -> [Int] -> Bool
isPath _ [] = False
isPath graph (x:xs) = elem x (nodes graph) && helper xs x
 where
     helper :: [Int] -> Int -> Bool
     helper [] _ = True
     helper (x:xs) currentParent = elem x (successors graph currentParent) && helper xs x

subseq :: [a] -> [[a]]
subseq [] = [[]]
subseq (x:xs) = map (x :) (subseq xs) ++ (subseq xs)

simplePaths :: Graph -> Int -> Node -> [Path]
simplePaths xs len n =  if not(elem n (nodes xs)) then error "Node is not present" else  filter(\ x -> length x == len+1 && isPath xs x  && head x == n ) (subseq(nodes xs))



