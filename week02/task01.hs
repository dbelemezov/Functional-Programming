main :: IO()
main = do
 print $ isPalindrome 1 == True
 print $ isPalindrome 6 == True
 print $ isPalindrome 1010 == False
 print $ isPalindrome 505 == True
 print $ isPalindrome 123321 == True
 print $ isPalindrome 654 == False
 print $ isPalindrome 121 == True
 print $ isPalindrome 12 == False
 print $ isPalindrome 120 == False
 print $ isPalindrome 12321 == True
 print $ isPalindrome 1221 == True

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

