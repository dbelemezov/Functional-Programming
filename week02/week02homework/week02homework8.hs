main :: IO()
main = do
 print $ maxMultiple 2 7 == 6
 print $ maxMultiple 3 10 == 9
 print $ maxMultiple 7 17 == 14
 print $ maxMultiple 10 50 == 50
 print $ maxMultiple 37 200 == 185
 print $ maxMultiple 7 100 == 98  
 print $ maxMultiple 7 10 == 7
 print $ maxMultiple 4 4 == 4
 print $ maxMultiple 10 7 

maxMultiple :: Int -> Int -> Int 
maxMultiple n m 
 | n<0 || m<=0 = error "n and/or m are negative"
 | n>m = error "the first number should be bigger"
 | mod m n == 0 = m 
 | otherwise = maxMultiple n (m - 1)

      