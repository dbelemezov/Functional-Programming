main :: IO()
main = do
 print $ sqAvg 5 0 == 12.5
 print $ sqAvg 10 13 == 134.5

sqAvg ::  Int -> Int -> Double 
sqAvg n m = fromIntegral (n ^ 2 + m ^ 2) / 2