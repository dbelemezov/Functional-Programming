import Data.Char

main ::IO()
main = do
 print $ sumSpecialPrimes 5 2 == 392
 print $ sumSpecialPrimes 5 3 == 107
 print $ sumSpecialPrimes 10 3 == 462

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes x y = sum(take x [ i | i <- [2..], all (\ x -> mod i x /= 0) [2 .. i - 1] && elem y (map digitToInt (show i))])