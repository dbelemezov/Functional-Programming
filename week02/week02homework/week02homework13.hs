main ::IO()
main = do
    print $ findSum1 0 2 10 -- == 3578 -- 510 + 1022 + 2046
    print $ findSum1 5 3 5 -- == 174 -- 26 + 50 + 98

findSum:: Int -> Int -> Int -> Int 
findSum a b n = helper a 0 a
 where
     helper :: Int -> Int -> Int -> Int
     helper a current result
      | current > n-1 = result
      | otherwise = helper a (current + 1)  (result +  (2^current)*b)

findSum2:: Int -> Int -> Int -> Int 
findSum2 a b n = helper a 0 a
 where
     helper :: Int -> Int -> Int -> Int
     helper a current result
      | current > n-2 = result
      | otherwise = helper a (current + 1)  (result +  (2^current)*b)

findSum3:: Int -> Int -> Int -> Int 
findSum3 a b n = helper a 0 a
 where
     helper :: Int -> Int -> Int -> Int
     helper a current result
      | current > n-3 = result
      | otherwise = helper a (current + 1)  (result +  (2^current)*b)

findSum:: Int -> Int -> Int -> Int 
findSum a b n = (findSum1 a b n) + (findSum2 a b n) + (findSum3 a b n)