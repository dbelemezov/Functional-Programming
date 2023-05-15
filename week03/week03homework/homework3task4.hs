main ::IO()
main = do
 print $ p 1 == 1
 print $ p 2 == 5
 print $ p 3 == 12
 print $ p 4 == 22
 print $ p 5 == 35
 print $ p 6 == 51

p :: Int -> Int
p n
 | n < 0 = error "n was negative"
 | n == 1 = 1
 | otherwise = helper 0 1
  where
      helper :: Int -> Int -> Int
      helper current result
       | current == n-1 = result
       | otherwise = result + helper (current + 1) ((current)*3 + 4)