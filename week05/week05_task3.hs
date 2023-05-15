import Data.Char
main :: IO()
main = do
 print $ specialSum 1 100 == 195

specialSum :: Int -> Int -> Int
specialSum x y = sum([ i | i <- [x..y], any (\ k -> 4*k + 1 == i) [1 .. y] && elem 6 (map digitToInt (show i))]) 