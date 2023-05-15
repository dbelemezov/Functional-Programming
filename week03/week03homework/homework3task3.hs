main ::IO()
main = do
 print $ takeK 5123783 3 == 512
 print $ takeK 123783 3 == 123
 print $ takeK 23783 3 == 237
 print $ subNum 123 5123783 == True -- x = 123, y = 5123783
 print $ subNum 0 0 == True
 print $ subNum 10 101 == True
 print $ subNum 101 101 == True
 print $ subNum 10 0 == False
 print $ subNum 1253 5123783 == False
 print $ subNum 12 0 == False

rev :: Int -> Int
rev n = helper n 0
 where
     helper :: Int -> Int -> Int
     helper leftOver result
      | leftOver < 10 = result * 10 + leftOver
      | otherwise = helper (div leftOver 10) (result * 10 + (mod leftOver 10))

countDigitsIter :: Int -> Int 
countDigitsIter 0 = 1
countDigitsIter n = helper n 0
 where 
     helper :: Int -> Int -> Int 
     helper 0 result = result
     helper n result
      | n < 0 = error "n was negative"
      | otherwise = helper (div n 10) (result + 1)

takeK :: Int-> Int-> Int
takeK y k 
 | k == 2 && countDigitsIter y == 3 = div y 10
 | otherwise= rev(mod (rev y) (10^k))

subNum :: Int -> Int -> Bool
subNum x y
 | x < 0 = error "x was negative"
 | x == y = True
 | y < 10^(countDigitsIter x) = False
 | takeK y (countDigitsIter x) == x = True
 | otherwise = subNum x (rev(div (rev y) 10))