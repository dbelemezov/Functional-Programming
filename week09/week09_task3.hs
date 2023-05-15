import Data.List
main :: IO()
main = do
 print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2), (ThreeD 6 5 4)] == (2.8284271247461903,ThreeD 4.0 5.0 6.0,ThreeD 6.0 5.0 4.0)

data Point a = ThreeD a a a
 deriving (Show, Eq)

distance :: (Floating a, RealFrac a) => Point a -> Point a -> a
distance (ThreeD x1 y1 z1) (ThreeD x2 y2 z2) =  sqrt $ (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) + (z2 - z1) * (z2 - z1)

correctLenght :: [[Point a]] -> [[Point a]]
correctLenght [] = []
correctLenght xs
 | length (head xs) == 2 = head xs : correctLenght (tail xs)
 | otherwise = correctLenght (tail xs)

getClosestDistance :: (Floating a, RealFrac a, Ord a, Enum a) => [Point a] -> (a, Point a, Point a)
getClosestDistance xs = (distance (head (foldr1 (\ c1 c2 -> if (distance (head c1) (last c1)) < (distance (head c2) (last c2)) then c1 else c2) (correctLenght (subsequences xs)))) 
                                  (last (foldr1 (\ c1 c2 -> if (distance (head c1) (last c1)) < (distance (head c2) (last c2)) then c1 else c2) (correctLenght (subsequences xs))))
                      , (head (foldr1 (\ c1 c2 -> if (distance (head c1) (last c1)) < (distance (head c2) (last c2)) then c1 else c2) (correctLenght (subsequences xs)))) 
                      , (last (foldr1 (\ c1 c2 -> if (distance (head c1) (last c1)) < (distance (head c2) (last c2)) then c1 else c2) (correctLenght (subsequences xs)))))

