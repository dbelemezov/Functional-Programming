main :: IO()
main = do
 print $ sumCubesPow 5 1 == 126
 print $ sumCubesPow 10 50 == 126000
 print $ sumCubesNoPow 5 1 == 126
 print $ sumCubesNoPow 10 50 == 126000

sumCubesPow :: Int -> Int -> Int
sumCubesPow n m = n ^ 3 + m ^ 3

sumCubesNoPow :: Int -> Int -> Int
sumCubesNoPow n m = n*n*n + m*m*m