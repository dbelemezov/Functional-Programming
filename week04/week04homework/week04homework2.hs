main :: IO()
main = do
 print $ isPresentRecNonPM 0 [] == False
 print $ isPresentRecNonPM 0 [1, 2, 3] == False
 print $ isPresentRecNonPM 0 [0, -1, 2] == True
 print $ isPresentRecPM 0 [] == False
 print $ isPresentRecPM 0 [1, 2, 3] == False
 print $ isPresentRecPM 0 [0, -1, 2] == True
 print $ isPresentFunc 0 [] == False
 print $ isPresentFunc 0 [1, 2, 3] == False
 print $ isPresentFunc 0 [0, -1, 2] == True
    
isPresentRecNonPM :: Int -> [Int] -> Bool
isPresentRecNonPM n xs    
 | xs == [] = False
 | last xs == n = True
 | otherwise = isPresentRecNonPM n (init xs)

isPresentRecPM :: Int -> [Int] -> Bool
isPresentRecPM n [] = False
isPresentRecPM n (x:xs) = n == x || isPresentRecPM n xs

isPresentFunc :: Int -> [Int] -> Bool
isPresentFunc n xs = elem n xs

