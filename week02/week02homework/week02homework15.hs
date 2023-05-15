main ::IO()
main = do
 print $ isSpecial 131 2 == True
 print $ isSpecial 472 2 == False
 print $ isSpecial 17197 2 == True
 print $ isSpecial 12234 3 == False
 print $ isSpecial 10113 3 == True
 print $ isSpecial 353 2 == False

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n
 | n < 1 = error "n was not natural"
 | otherwise = helper 2
  where
      helper :: Int -> Bool
      helper currentDivisor
       | currentDivisor == n = True
       | mod n currentDivisor == 0 = False
       | otherwise = helper (currentDivisor + 1)

isSpecial :: Int -> Int -> Bool
isSpecial m n
 | m < 0 || n < 0 = error "a and/or b are negative"
 | otherwise = helper m
  where
      helper :: Int -> Bool
      helper current
       | isPrime (current) && current <= 10^n = True
       | isPrime (mod current (10^n)) = helper (div current 10)
       | otherwise = False  