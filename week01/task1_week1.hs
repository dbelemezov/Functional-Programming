main :: IO()
main = do
 print $ "Hello World"
 print $ min 5 6 == 5
 print $ minIf (-60) (-15) == (-60)
 print $ minIf 15 60 == 15
 print $ minIf 60 15 == 15
 print $ minGuard 15 60 == 15
 print $ minGuard 60 15 == 15
 print $ lastDigit 154 == 4
 print $ quotientWhole 64 2 == 32
 print $ quotientWhole 154 17 == 9
 print $ divWhole 154 17 == 9.058823529411764
 print $ removeLastDigit 154 == 15 
 print $ divReal 154 10 == 15.4
 print $ quotientReal 154.21 17.17 == 9
 print $ averageWhole 5 1542 == 773.5
 print $ areNotEqualOneLine 5 2 == True
 print $ areNotEqualOneLine 5 5 == False
 print $ areNotEqualGuards 5 2 == True
 print $ areNotEqualGuards 5 5 == False
 print $ inside 1 5 4 == True -- a = 1, b = 5, x = 4
 print $ inside 5 1 4 == True
 print $ inside 10 50 20 == True
 print $ inside 10 50 1 == False

minIf :: Int -> Int -> Int 
minIf n m = if n<m then n else m

minGuard :: Int -> Int -> Int
minGuard x y 
 | x<y=x
 | x>=y=y 

lastDigit :: Int -> Int
lastDigit x = mod x 10

quotientWhole :: Int -> Int -> Int
quotientWhole x y = if y==0 then error "y was 0" else div x y




divWhole :: Int -> Int -> Double 
divWhole x y = (fromIntegral x) / (fromIntegral y)

removeLastDigit :: Int -> Int
removeLastDigit x = div x 10

divReal :: Double -> Double -> Double
divReal x y = x/y

quotientReal :: Double -> Double -> Int
quotientReal x y = round $ x/y

averageWhole :: Int -> Int -> Double
averageWhole x y = fromIntegral (x+y)/2

areNotEqualOneLine :: Int -> Int -> Bool
areNotEqualOneLine x y = x/=y -- not $ x==y

areNotEqualGuards :: Int -> Int -> Bool
areNotEqualGuards x y
 | x==y = False
 | otherwise = True 

 inside :: Int -> Int -> Int -> Bool
 inside a b x = min a b <= x && x <= max a b