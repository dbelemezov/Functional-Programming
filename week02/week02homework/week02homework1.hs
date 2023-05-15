main :: IO()
main = do
 --print $ countDigitsRec (-13) -- error "n was negative"
 print $ countDigitsRec 12345 == 5
 print $ countDigitsRec 123 == 3
 print $ countDigitsRec 0 == 1
 print $ countDigitsIter 12345 == 5
 print $ countDigitsIter 123 == 3
 print $ countDigitsIter 0 == 1

countDigitsRec :: Int -> Int 
countDigitsRec n 
 | n < 0 = error "n was negative"
 | n < 10 = 1
 | n == 0 = 0
 | otherwise = 1 + countDigitsRec (div n 10)

countDigitsIter :: Int -> Int 
countDigitsIter 0 = 1
countDigitsIter n = helper n 0
 where 
     helper :: Int -> Int -> Int 
     helper 0 result = result
     helper n result
      | n < 0 = error "n was negative"
      | otherwise = helper (div n 10) (result + 1)


