import Data.List
main :: IO()
main = do
 print $ coldestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"
    
type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
 deriving (Show, Eq)
data Country = Country Name Capital [City]
 deriving (Show, Eq)

cityTemp ::  City -> AvgYearlyTemperature
cityTemp (City a b c) = c

cityTemps :: [City] -> [AvgYearlyTemperature]
cityTemps [] = []
cityTemps xs = cityTemp (head xs) : cityTemps (tail xs) 

avgTemp :: [AvgYearlyTemperature] -> AvgYearlyTemperature
avgTemp xs = (sum xs) / fromIntegral (length xs) 

countryAvgTemps :: Country  -> AvgYearlyTemperature
countryAvgTemps (Country a b c) = avgTemp $ cityTemps c

coldestCountryName :: Country -> Name
coldestCountryName (Country a b c) = a

coldestCapital :: [Country] -> Name
coldestCapital xs = coldestCountryName $ foldr1 (\ c1 c2 -> if countryAvgTemps c1 <= countryAvgTemps c2 then c1 else c2) xs
