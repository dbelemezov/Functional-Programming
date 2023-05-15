main :: IO()
main = do
 print $ factPM 11 == 39916800
 -- print $ factPM (-5) --ne bachkaa
 print $ factGuards 11 == 39916800
 print $ factIter 11 == 39916800 --ne bachka
 


factPM :: Int -> Int 
factPM 0 = 1
factPM n = if n<0 then error "n was negative" else n*factPM(n-1)

factGuards :: Int -> Int
factGuards n
 |n<0 = error "n was negative"
 | n==0 = 1
 |otherwise = n*factGuards(n-1)

 factIter :: Int -> Int 
 factIter n = helper n 1
 | where
     helper:: Int -> Int -> Int 
     helper 0 result = result 
     helper n result
     |n<0 = error "n was  negative"
     |otherwise = helper (n-1)(result*n)