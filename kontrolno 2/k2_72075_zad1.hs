main :: IO()
main = do
 print $ (getIndices [2, 7, 11, 15]) 9 -- → (0,1) -- 2 + 7 = 9
 print $(getIndices [3, 2, 4]) 6--  → (1,2)
 print $(getIndices [3, 3]) 6 -- → (0,1)
--ne trygva nishto

getIndices :: [Int] -> (Int -> (Int, Int))
getIndices xs = (\n -> [b | (a,b) <- zip xs [0 ..],(x,y)<- zip xs xs ])