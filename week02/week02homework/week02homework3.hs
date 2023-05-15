main :: IO()
main = do
 print $ sumPrimeDivs 6 == 5 -- 2 + 3
 print $ sumPrimeDivs 18 == 5 -- 2 + 3
 print $ sumPrimeDivs 45136 == 53
 print $ sumPrimeDivs 11 == 11


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


sumPrimeDivs :: Int -> Int
sumPrimeDivs n
 | n < 0 = error "n was negative"
 | otherwise = helper 1
  where
      helper :: Int -> Int
      helper currentDivisor
       | isPrime n = n
       | currentDivisor >= n = 0 
       | mod n currentDivisor == 0 && isPrime currentDivisor = currentDivisor + helper (currentDivisor + 1)
       | otherwise = helper (currentDivisor + 1)            

