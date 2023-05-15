main ::IO()
main = do
 print $ countPalindromes 5 13 == 5  -- 6 7 8 9 11
 print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11  
 print $ countPalindromes 70 78 == 1 
 print $ countPalindromes 98 112 == 3
 print $ countPalindromes 78 70 == 1

rev :: Int -> Int
rev n
 | n < 0 = error "n was negative"
 | otherwise = helper n 0
  where
      helper :: Int -> Int -> Int
      helper leftOver result
       | leftOver < 10 = result * 10 + leftOver
       | otherwise = helper (div leftOver 10) (result * 10 + mod leftOver 10)

isPalindrome :: Int -> Bool
isPalindrome n = n == rev n

countPalindromes :: Int -> Int -> Int 
countPalindromes 0 0 = 0
countPalindromes a b
 | a < 0 || b < 0 = error "a and/or b are negative"
 | b<a = countPalindromes b a
 | otherwise = helper (a+1) 0
  where
     helper :: Int -> Int -> Int 
     helper current counter
       | current >= b = counter
       | isPalindrome current = helper (current + 1) (counter+1)
       | otherwise = helper (current + 1) counter