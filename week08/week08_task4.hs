import Data.List
main :: IO()
main = do
 print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587]
 print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0

data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
 deriving (Show, Eq)

perimeter :: (Num a,Eq a, Floating a) => Shape a -> a
perimeter (Circle r) = 2*r*pi
perimeter (Rectangle a b) = 2*a+2*b
perimeter (Triangle a b c) = a + b + c
perimeter (Cylinder r h) = 4*r + 2*h 

area :: (Num a,Eq a, Floating a) => Shape a -> a
area (Circle r) = r*r*pi
area (Rectangle a b) = a*b
area (Cylinder r h) = 2*pi*r*h + 2*pi*r*r
area (Triangle a b c) = sqrt(p*(p-a)*(p-b)*(p-c))
 where
     p = perimeter(Triangle a b c)/2

getAreas :: (Num a,Eq a, Floating a) => [Shape a] ->[a]      
getAreas xs =  map(\x->area x) xs

maxArea :: (Num a,Eq a, Floating a,Ord a) => [Shape a] -> Shape a
maxArea xs = foldl1 (\ x y -> if (area x) > (area y) then x else y) xs

