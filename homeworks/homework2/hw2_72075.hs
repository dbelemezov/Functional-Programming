main :: IO()
main = do
 print $ countRats ")1)1)1)1 P" == 0 
 print $ countRats "P 1( 1( )1 1(" == 1
 print $ countRats " P 1( 1( )1 1(" == 1
 print $ countRats ")1)1)1)1P)1)11("  == 2
 print $ countRats "1()1)1)11(1()1)1P)11()1)1)11(1(1(1(" == 7

 print $ josephus [1,2,3,4,5,6,7] 3 == [3,6,2,7,5,1,4]
 print $ josephus [1,2,3,4,5,6,7,8,9,10] 1 == [1,2,3,4,5,6,7,8,9,10] 
 print $ josephus [1,2,3,4,5,6,7,8,9,10] 2 == [2,4,6,8,10,3,7,1,9,5]
 print $ josephus "fpFMIsu" 4 == "MfsIuFp"
 --print $ josephus [1,2,3,4,5,6,7] (-1) -- error

--task1
cutWhitespace :: String -> String
cutWhitespace xs = filter (\ x -> x/=' ') xs

splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

function1 :: String -> Int
function1 xs = length(filter (\ x -> x ==')') xs)

function2 :: String -> Int
function2 xs = length(filter (\ x -> x =='(') xs)

function3 :: String -> Int
function3 xs = function2(fst(splitAtFirst 'P' xs)) + function1(snd(splitAtFirst 'P' xs))

countRats :: String -> Int
countRats [] = error "invalid input"
countRats xs = helper (cutWhitespace xs) 
 where
     helper :: String -> Int
     helper xs
      | head xs == 'P' = function1 xs
      | last xs == 'P' = function2 xs
      | otherwise = function3 xs

--task2
      --josephus :: [Int] -> (Int -> [Int])
josephus xs  = \ a -> create xs a

--remove :: [Int] -> Int -> [Int]
remove xs num = [x | x <- xs , x /= num]
 
--create :: [Int] -> Int -> [Int]
create xs k = helper xs k 1 [] xs
 where 
--helper :: [Int] -> Int -> Int-> [Int] -> [Int] -> [Int]
  helper leftOver k ind result xs
   | k < 0 =  error "k was not natural"
   | xs == [] = result
   | ind < k && head xs /= last xs = helper leftOver k (ind+1) result (tail xs)
   | ind < k && head xs == last xs = helper leftOver k (ind+1) result leftOver
   | ind == k && head xs /= last xs = helper (remove leftOver (head xs)) k 1 (result ++ [head xs]) (tail xs)
   | ind == k && head xs == last xs = helper (remove leftOver (head xs)) k 1 (result ++ [head xs]) (remove leftOver (head xs))