main ::IO()
main = do
 -- task 1a
 print $ safePrimesCount 20 100 == 4
 print $ safePrimesCount 1 983 == 25
 print $ safePrimesCount 167 1892 == 28
 print $ safePrimesCount 1678 20097 == 155
 -- task 1b
 print $ specialSum 3 20 == 2205 
 print $ specialSum 5 31 == 665723
 print $ specialSum 8 10 == 545925272
 print $ specialSum 10 128 == 11135248639990 
 -- task 2
 print $ validate 1714 == False
 print $ validate 12345 == False
 print $ validate 891 == False
 print $ validate 123 == False
 print $ validate 2121 == True
 print $ validate 4736778291034 == True
 print $ validate 4485756008412 == True
 print $ validate 4485756008422 == False
 print $ validate 4214154976719 == True

-- task 1a
-- bavna e, okolo 40 sekundi

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = helper 2
    where
        helper :: Int -> Bool
        helper d
            | d == n = True
            | mod n d == 0 = False
            | otherwise = helper (d + 1)

safePrimesCount :: Int -> Int -> Int
safePrimesCount a b = helper b 0
 where
     helper :: Int -> Int -> Int 
     helper b res
        | b < a = res
        | isPrime b && isPrime (div (b-1) 2) = helper (b-1) (res+1)
        | otherwise = helper (b-1) res

-- task 1b
-- vkluchva isPrime ot 1a

checkNumber :: Int -> Int -> Bool
checkNumber n b
 | not(isPrime n) = False
 | isPrime n && (2^n - 1) > b = True
 | otherwise = checkNumber(n + 1) b

specialSum :: Int -> Int-> Int
specialSum a b = helper 2 a 0 
 where
     helper :: Int -> Int -> Int -> Int
     helper current a result 
      | a == 0 = result 
      | checkNumber current b= result + helper (current + 1) (a - 1) (2^current - 1)
      | otherwise =  helper (current + 1) a result


-- task 2

countDigitsRec :: Int -> Int
countDigitsRec n
 | n < 0 = error "n was negative"
 | n < 10 = 1
 | n == 0 = 0
 | otherwise = 1 + countDigitsRec (div n 10)

rev :: Int -> Int
rev n = helper n 0
 where
     helper :: Int -> Int -> Int
     helper leftOver result
      | leftOver < 10 = result * 10 + leftOver
      | otherwise = helper (div leftOver 10) (result * 10 + (mod leftOver 10))

doubleUp :: Int -> Int
doubleUp n
 | n < 0 = error "n was negative"
 | n*2 > 9 = (mod (n*2) 10) + (div (n*2) 10)
 | otherwise = n*2

doubleEverySecondNumberEven :: Int -> Int
doubleEverySecondNumberEven n = helper n 0
 where
     helper :: Int -> Int -> Int
     helper leftOver result
      | leftOver <= 0 = rev(result)
      | mod (countDigitsRec leftOver) 2 == 0 = helper (div leftOver 10) ((result*10 + mod leftOver 10))
      | otherwise = helper (div leftOver 10) (result*10 + (doubleUp (mod leftOver 10)))

doubleEverySecondNumberOdd :: Int -> Int
doubleEverySecondNumberOdd n = helper n 0
 where
     helper :: Int -> Int -> Int
     helper leftOver result
      | leftOver <= 0 = rev(result)
      | mod (countDigitsRec leftOver) 2 == 0 = helper (div leftOver 10) (result*10 + doubleUp (mod leftOver 10))
      | otherwise = helper (div leftOver 10) (result*10 +  mod leftOver 10)     

sumDigitsRec :: Int -> Int
sumDigitsRec n
 | n < 0 = error "n was not positive"
 | n < 10 = n
 | otherwise = mod n 10 + sumDigitsRec (div n 10)

isEvenOdd :: Int -> Int
isEvenOdd n
 | mod (countDigitsRec n) 2 == 0 = sumDigitsRec(doubleEverySecondNumberEven n)
 | otherwise = sumDigitsRec(doubleEverySecondNumberOdd n)

validate :: Int -> Bool
validate n
 | countDigitsRec n > 16 = False
 | mod (isEvenOdd n) 10 == 0 = True
 | otherwise = False 
