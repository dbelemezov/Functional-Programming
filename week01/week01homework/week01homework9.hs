main :: IO()
main = do
 print $ rev 1 == 1
 print $ rev 123 == 321
 print $ rev 987654321 == 123456789  

rev :: Int -> Int 
rev n = helper n 0
 where 
     helper :: Int -> Int -> Int 
     helper 0 result = result
     helper n result
      | n < 0 = error "n was negative"
      | otherwise = helper (div n 10) (result*10 + mod n 10)




