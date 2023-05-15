import Data.List
main :: IO()
main = do
 print $ (applyN (\ x -> 2 * x) 5) 2 == 64
 print $ (applyN (\ x -> div x 10) 2) 100 == 1

applyN ::(Num a, Enum a) => (a -> a) -> Int -> (a -> a)
applyN f n = (\ x -> (iterate f x) !! n)


