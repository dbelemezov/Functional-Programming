import Data.Char
import Data.List

main :: IO()
main = do
 print $ sumOfEvenly 1 10 == 41 -- 2+3+5+6+7+8+10
 print $ sumOfEvenly 5 20 == 175 

isEven :: Int -> Bool
isEven n = mod n 2 == 0

countDivs :: Int -> Int 
countDivs n = length $ filter (\x -> mod n x == 0) [1 ..  n]

sumOfEvenly :: Int -> Int -> Int
sumOfEvenly a b = sum $ filter (\x -> isEven(countDivs x)) [min a b .. max a b]