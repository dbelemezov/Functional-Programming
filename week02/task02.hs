main :: IO()
main = do
    print $ countOccurences 121 1 == 2
    print $ countOccurences (-5) 1

countOccurences :: Int -> Int -> Int
countOccurences n findNum
 | n < 10 = if n == findNum then 1 else 0
 | mod n 10 == findNum = 1 + countOccurences (div n 10) findNum
 | otherwise = countOccurences (div n 10) findNum