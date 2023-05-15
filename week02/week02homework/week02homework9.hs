main :: IO()
main = do
 print $ removeD 1 656 == 656
 print $ removeD 5 656 == 66
 print $ removeD 6 656 == 5
 print $ removeD 0 606 == 66
 print $ removeD 0 600 == 6
 print $ removeD 6 600 == 0
 print $ removeD 2 124 == 41

removeD :: Int -> Int -> Int 
removeD n m
 | n<0 || m<0 = error "n and/or m are negative"
 | otherwise = helper m 0
  where 
     helper :: Int -> Int -> Int 
     helper m current 
       | m <= 0 = current 
       | mod m 10 == n = helper (div m 10) current
       | otherwise = helper (div m 10) (current*10 + mod m 10)



