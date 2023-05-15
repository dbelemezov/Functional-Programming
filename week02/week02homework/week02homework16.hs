main ::IO()
main = do
 print $ reverseOrdSuff 37563 == 36
 print $ reverseOrdSuff 32763 == 367
 print $ reverseOrdSuff 32567 == 7
 print $ reverseOrdSuff 32666 == 6
 print $ reverseOrdSuff 65385 == 58
 print $ reverseOrdSuff 63826 == 6
 print $ reverseOrdSuff 4321 == 1234

reverseOrdSuff:: Int -> Int
reverseOrdSuff n = helper n 0
 where
     helper :: Int -> Int -> Int
     helper 0 result = result
     helper n result
      | n < 0 = error "n was negative"
      | mod n 10 >= (div (mod n 100) 10) = (result * 10 + (mod n 10))
      | otherwise = helper (div n 10) (result * 10 + mod n 10)
