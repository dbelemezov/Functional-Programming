main :: IO()
main = do
 print $ isPerfect 1 == False -- the sum of the divisors is 0, because of the hint
 print $ isPerfect 6 == True -- 1 + 2 + 3 = 6 = 6
 print $ isPerfect 495 == False
 print $ isPerfect 33550336 == True
 -- raboti programata, obache zaradi tozi sluchai se bavi
 print $ sumDivs 6 == 6
 print $ sumDivs 10 == 8

sumDivs :: Int -> Int
sumDivs 0 = 0
sumDivs n
 | n < 0 = error "n was negative"
 | otherwise = helper 1
  where
      helper :: Int -> Int
      helper currentDivisor
       | currentDivisor >= n = 0 -- currentDivisor
       | mod n currentDivisor == 0 = currentDivisor + helper (currentDivisor + 1)
       | otherwise = helper (currentDivisor + 1)

isPerfect :: Int -> Bool 
isPerfect 1 = False
isPerfect n 
 | n < 1 = error "n wasn't natural"
 | n == sumDivs n = True 
 | otherwise = False
       