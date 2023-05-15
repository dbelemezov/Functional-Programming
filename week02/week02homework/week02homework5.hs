main :: IO()
main = do
 print $ hasIncDigits 1244 == True
 print $ hasIncDigits 12443 == False  

hasIncDigits :: Int -> Bool
hasIncDigits n
    | n < 10    = True
    | otherwise = helper (div n 10) (mod n 10)
    where
        helper num lastDigit
            | num < 10 = num < lastDigit
            | (mod num 10) > lastDigit = False
            | otherwise  = helper (div num 10) (mod num 10)   