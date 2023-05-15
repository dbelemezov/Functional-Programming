main :: IO()
main = do
 print $ closestToAverage [(Temp 1 23.6),(Temp 6 24.2),(Temp 11 24.2),(Temp 16 21.2),(Temp 21 23.8),(Temp 26 26.5),(Temp 31 24.5)]

data Measuring = Temp Int Float

getDay :: Measuring -> Int
getDay (Temp day temperature) = day

closestToAverage :: [Measuring] -> Int
closestToAverage [] = 0
closestToAverage measuring = getDay (foldl1 (\ (Temp d1 temp1) (Temp d2 temp2) -> 
                                    if abs (temp1 - avgTemp) < abs (temp2 - avgTemp)
                                    then (Temp d1 temp1) else (Temp d2 temp2)) measuring)
                                    where
                                        avgTemp = avg [t | (Temp d t) <- measuring]
                                            where
                                                avg :: [Float] -> Float
                                                avg [] = 0
                                                avg temps = (sum temps) / (fromIntegral (length temps))
                                              
