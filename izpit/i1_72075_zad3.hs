main :: IO()
main = do
 print $ cook [ApplePie, ApplePie, Burger, Chicken, Chicken, ApplePie] == [Sunny, Rainy, Rainy, Sunny, Rainy]
 print $ cook [ApplePie, Burger, Chicken, Chicken, ApplePie, Burger] == [Rainy,Rainy,Sunny,Rainy,Rainy]

-- pravq si 2 algebrichni tipa, ediniq e hrana, drugiq e vreme
data Hrana = ApplePie | Burger | Chicken
 deriving (Show, Eq)
data Vreme = Sunny | Rainy
 deriving (Show, Eq)

cook :: [Hrana] -> [Vreme]
cook [a,b] = if (a==b) then [Sunny] else [Rainy] 
cook (a:b:as) 
 | a==b = Sunny : cook (b:as)
 | otherwise = Rainy : cook (b:as) 