import Data.Char
import Data.List

main :: IO()
main = do
 print $ persistence 39 -- == (3,[27,14,4]) -- 3*9=27, 2*7=14, 1*4=4
 print $ persistence 999 -- == (4,[729,126,12,2]) -- 9*9*9=729, 7*2*9=126,
 print $ persistence 126 -- == (2,[12,2]) -- 1*2*6=12, 1*2=2
 print $ persistence 4 -- == (1,[4])

productN :: Int -> Int 
productN n= if n<10 then n else mod n 10 * productN (div n 10)

creatingList2 :: Int -> [Int]
creatingList2 n = if n>10 then productN n :creatingList2 (productN n) else []

--creatingList :: Int -> [Int]
--creatingList n = [productN n, productN (productN n) .. 1 ]

persistence :: Int -> (Int, [Int])
persistence n = (length (creatingList2 n), creatingList2 n)