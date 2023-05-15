main ::IO()
main = do
 print $ truncatablePrime 3797 == True -- 3797, 379, 37 and 3 are all prime
 print $ truncatablePrime 47 == False -- 47 is prime, but 4 is not  
 print $ truncatablePrime 346 == False 

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

truncatablePrime :: Int -> Bool 
truncatablePrime n
 | n<0 = error "n was negative"
 | otherwise = helper n
  where 
     helper :: Int -> Bool 
     helper n 
      | n<10 && isPrime (n) = True
      | isPrime (n) = helper (div n 10)
      | otherwise = False  
