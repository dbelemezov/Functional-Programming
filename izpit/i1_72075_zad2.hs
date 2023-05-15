main :: IO()
main = do
 print $ (speak "gate") 't' -- "ga1e"
 print $ (speak "This is a test") 'i' -- "Th11s 8s a test"
 print $ (speak "iiiiiii") 'i' -- "6543210"

speak :: String -> (Char -> String)

