import Data.Char
import Data.List

main :: IO()
main = do
 print $ kthMaxMin [-1] 1 == -1
 print $ kthMaxMin [-1,0,-1,0,-2,3,1,-1] 3 -- error
 print $ kthMaxMin [-1,-5,-6,-6,-6,-6] 2 == -5
 print $ kthMaxMin [1,2,3,4,-5,6,7,-2,-1,0] 2 == -2

kthMaxMin :: [Int] -> (Int -> Int)
