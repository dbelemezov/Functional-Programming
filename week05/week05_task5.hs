main :: IO()
main = do
 print ((pairCompose [(\x -> x+1),(\x -> x+2),(\x -> x+3)]) 1) -- == 8
   
pairCompose :: [Int -> Int] -> (Int -> Int)
pairCompose [] x = 0                                        
pairCompose (f:g:fs) x = helper + pairCompose fs x          
 where helper = (f . g) x
pairCompose (f:fs) x = (f . (\x -> x)) x