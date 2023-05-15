main :: IO()
main = do
 print $ isArithmentic [3] == True
 print $ isArithmentic [3, 5] == True
 print $ isArithmentic [1, 2, 3, 4, 5] == True
 print $ isArithmentic [3, 5, 7, 9, 11] == True
 print $ isArithmentic [3, 5, 8, 9, 11] == False
    
isArithmentic :: [Int] -> Bool
isArithmentic [] = False
isArithmentic [x] = True
isArithmentic [x,y] = True
isArithmentic (x:y:z:xs) = (x - y) == (y - z) && isArithmentic (y:z:xs)