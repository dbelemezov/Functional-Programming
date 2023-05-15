main ::IO()
main = do
 print $ isSquare 1 == True
 print $ isSquare 2 == False
 print $ isSquare 4 == True
 print $ isSquare 17 == False
 print $ isSquare 256 == True
 print $ isSquare 2500 == True


isSquare :: Int -> Bool
isSquare n
 | n < 0 = error "n was negative"
 | otherwise = helper 1 
  where
      helper :: Int -> Bool
      helper current
       | current*current > n = False
       | current*current == n && mod n current == 0 && (div n current == current) = True
       | otherwise = helper (current + 1)