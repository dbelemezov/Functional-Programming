import Data.List
main :: IO()
main = do
 print $ listLeaves [(1, 2, 3), (2, 4, 5)] -- == [4, 3, 5] -- tuk vryshtam [3,4,5], ama e syshtoto
 print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
 print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]

allLeaves :: [(Int, Int, Int)] -> [Int]
allLeaves = concatMap (\(x, y, z) -> [y, z])

allNodes :: [(Int, Int, Int)] -> [Int]
allNodes xs = [ x | (x, y, z) <- xs]

isLeaf :: Int -> [Int] -> Bool
isLeaf _ [] = False
isLeaf x xs = elem x xs

listLeaves :: [(Int, Int, Int)] -> [Int]
listLeaves xs = filter (\ x -> not(isLeaf x (allNodes xs))) (allLeaves xs)
