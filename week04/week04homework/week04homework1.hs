main :: IO()
main = do
 print $ mySumRecNonPM [] == 0
 print $ mySumRecNonPM [1, 2, 3] == 6
 print $ mySumRecPM [] == 0
 print $ mySumRecPM [1, 2, 3] == 6
 print $ mySumFunc [] == 0
 print $ mySumFunc [1, 2, 3] == 6
    
mySumRecNonPM :: [Int] -> Int
mySumRecNonPM xs
 | xs == [] = 0
 | otherwise = (last xs) + mySumRecNonPM (init xs)

mySumRecPM :: [Int] -> Int
mySumRecPM [] = 0
mySumRecPM (x:xs) = (last  (x:xs)) + mySumRecPM (init (x:xs))

mySumFunc :: [Int] -> Int
mySumFunc xs = sum xs