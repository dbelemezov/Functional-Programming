main ::IO()
main = do
 print $ mySin 100 1 == 0.8414709848078965 -- n = 100, x = 1
 print $ mySin 100 0.5 == 0.479425538604203

factPM :: Double -> Double
factPM 0 = 1
factPM n = if n < 0 then error "n was negative" else n * factPM (n - 1)

mySin :: Double -> Double -> Double
mySin n x 
 | n < 0 = error "Invalid input"
 | otherwise = helper n (2*n - 1) 0 
  where 
     helper :: Double -> Double -> Double -> Double
     helper currentElement power result
      | currentElement == 0 = result
      | otherwise = result + helper (currentElement - 1) (power - 2) (((-1)**(currentElement + 1))*((x**power)/(factPM power)))      
