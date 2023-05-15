main :: IO()
main = do
 print $ myPoly [2.7, 3.0 ..] 2.2 3 == -0.4399999999999998

myPoly :: [Double] -> (Double -> Int -> Double)
myPoly xs = (\x y -> foldr1 (*) (map (\xn -> x - xn )(take y xs)))