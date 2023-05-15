main :: IO()
main = do
 print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] -- == "Bulgaria"

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
 deriving (Show, Eq)
data Country = Country Name Capital [City]
 deriving (Show, Eq)

cityElevation :: City -> Elevation
cityElevation (City a b c) = b

cityElevations :: [City] -> [Elevation]
cityElevations [] = []
cityElevations xs = cityElevation (head xs) : cityElevations (tail xs) 

avgElevation :: [Elevation] -> Double
avgElevation xs = fromIntegral(sum xs) / fromIntegral (length xs) 

countryAvgElevations :: Country  -> Double
countryAvgElevations (Country a b c) = avgElevation $ cityElevations c

highestCountryName :: Country -> Name
highestCountryName (Country a b c) = a

highestCapital :: [Country] -> Name
highestCapital xs = highestCountryName $ foldr1 (\ c1 c2 -> if countryAvgElevations c1 <= countryAvgElevations c2 then c2 else c1) xs
