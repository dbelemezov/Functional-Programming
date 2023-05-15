main :: IO()
main = do
 print $ isInteresting 410 == True
 print $ isInteresting 212 == False
 print $ isInteresting 70 == True 
 print $ isInteresting 5 == True 

sumDigitsIter :: Int -> Int 
sumDigitsIter n = helper n 0
 where 
     helper :: Int -> Int -> Int 
     helper 0 result = result
     helper n result
      | n < 0 = error "n was negative"
      | otherwise = helper (div n 10) (result + mod n 10)

isInteresting :: Int -> Bool 
isInteresting n 
 | n<0 = error "n was negative"
 | mod n (sumDigitsIter n) == 0 = True 
 | otherwise = False  
 