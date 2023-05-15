import Data.Char
import Data.List

main :: IO()
main = do
 print $ isAscending 0 == True
 print $ isAscending 10 == False
 print $ isAscending 123 == True
 print $ isAscending 1233 == True
 print $ isAscending 12332 == False   

isAscending :: Int -> Bool
isAscending n = (map digitToInt (show n)) == sort (map digitToInt (show n))

