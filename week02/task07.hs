  
main :: IO()
main = do
    print $ sumDivs 0 == 0
    print $ sumDivs 1 == 1
    print $ sumDivs 6 == 12 -- 1 + 2 + 3 + 6
    print $ sumDivs 12345 == 19776

sumDivs :: Int -> Int
sumDivs 0 = 0
sumDivs n
 | n < 0 = error "n was negative"
 | otherwise = helper 1
  where
      helper :: Int -> Int
      helper currentDivisor
       | currentDivisor >= n = n -- currentDivisor
       | mod n currentDivisor == 0 = currentDivisor + helper (currentDivisor + 1)
       | otherwise = helper (currentDivisor + 1)