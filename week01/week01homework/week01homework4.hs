main :: IO()
main = do
 print $ myGcd 5 13 == 1
 print $ myGcd 13 1235 == 13
 print $ myGcd 5 5 == 5
 print $ myGcd 5 0 == 5
 print $ myGcd 0 3 == 3

myGcd :: Int -> Int -> Int
myGcd n m
 | n==0 = m
 | m==0 = n
 | otherwise = myGcd m (mod n m)
