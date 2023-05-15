main ::IO()
main = do
 print $ sumSpecialPrimes 5 2 == 392
 print $ sumSpecialPrimes 5 3 == 107
 print $ sumSpecialPrimes 10 3 == 462

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

countOccurences :: Int -> Int -> Int
countOccurences n findNum
 | n < 10 = if n == findNum then 1 else 0
 | mod n 10 == findNum = 1 + countOccurences (div n 10) findNum
 | otherwise = countOccurences (div n 10) findNum

sumSpecialPrimes:: Int -> Int -> Int
sumSpecialPrimes a b = helper 1 0 0
 where
     helper :: Int -> Int -> Int -> Int
     helper current result counter
      | counter == a = result 
      | isPrime(current) && countOccurences (current) b > 0 = helper (current + 1) (result + current) (counter + 1)
      | otherwise = helper (current + 1) result counter