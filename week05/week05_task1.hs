main :: IO()
main = do
 print $ getPalindromes 132465 == 8
 print $ getPalindromes 654546 == 8
 print $ getPalindromes 100001 == 100012
 print $ getPalindromes 21612 == 21614
 print $ getPalindromes 26362 == 26364

rev :: Int -> Int
rev n = read $ reverse $ show n

isPalindrome :: Int -> Bool
isPalindrome n = n == rev n    

sumDivs :: Int -> [Int]
sumDivs n =  filter (\ x -> mod n x == 0 && isPalindrome x) [2..n]

getPalindromes :: Int -> Int
getPalindromes n = head (sumDivs n) + last (sumDivs n)