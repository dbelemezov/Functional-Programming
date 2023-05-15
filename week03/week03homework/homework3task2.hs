main ::IO()
main = do
 print $ calcSeriesSum 1 0 == -2.0 -- x = 1, n = 0
 print $ calcSeriesSum 1 1 == -0.6666666666666667
 print $ calcSeriesSum 1 2 == -1.2000000000000002
 print $ calcSeriesSum 1 3 == -1.047619047619048
 print $ calcSeriesSum 1 4 == -1.0814814814814817
 print $ calcSeriesSum 1 5 == -1.0753246753246755
 print $ calcSeriesSum 1 6 == -1.0762718762718764

calcSeriesSum :: Double -> Double -> Double
calcSeriesSum x n
 | n < 0 = error "n was negative"
 | n == 0 = -2
 | otherwise = helper x 3 1 0 0
 where
     helper :: Double -> Double -> Double -> Double -> Double -> Double 
     helper x delitelhelp delitel current result
      | current == n + 1 = result
      | otherwise = result + helper x (delitelhelp + 2) (delitel*delitelhelp) (current+1) (((-1)**(current+1))*(((2**(current + 1))*(x**current)) / delitel))

