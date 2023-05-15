main :: IO()
main = do
 print $ digitalRoot 16 == 7
 print $ digitalRoot 942 == 6
 print $ digitalRoot 132189 == 6
 print $ digitalRoot 493193 == 2

sumDigitsIter :: Int -> Int
sumDigitsIter n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper n result 
     | n == 0 = result
     | otherwise = helper (div n 10) (result + mod n 10)

digitalRoot :: Int -> Int
digitalRoot x
 | x < 0 = error "x was negative"
 | x < 10 = sumDigitsIter(x)
 | otherwise = digitalRoot(sumDigitsIter x)
 