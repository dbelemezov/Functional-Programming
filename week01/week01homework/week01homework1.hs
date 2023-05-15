main :: IO()
main = do
 print $ isEvenIf 2 == "Yes"
 print $ isEvenIf 15452 == "Yes"
 print $ isEvenIf 321 == "No"
 print $ isEvenGuards 2 == "Yes"
 print $ isEvenGuards 15452 == "Yes"
 print $ isEvenGuards 321 == "No"

-- ne se seshtam neshto kak po drug nachin da izkaram tova yes/no
isEvenIf :: Int -> String
isEvenIf n = if mod n 2 == 0 then "Yes" else "No"

isEvenGuards :: Int -> String
isEvenGuards n
 | mod n 2 == 0 = "Yes"
 | otherwise = "No"