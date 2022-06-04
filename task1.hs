main :: IO ()
main = do
    print $ sumCountsIter 1 1
    print $ sumCountsIter 5123 1
    print $ sumCountsIter 1234 8
    print $ sumCountsIter 5555 5
    print $ sumCountsIter 65432 6
    print $ sumCountsIter 70000 1
    print $ sumCountsIter 123321 1
    
    --will output 0
    print $ sumCountsIter 8 9

    --returns error aswell
    --print $ sumCountsIter 5123 10
    print $ sumCountsIter 5123 (-1)

--function to count the number of digits that correspond to the interval
countDigitsNumber :: (Num p, Integral a) => a -> a -> p
countDigitsNumber 0 _ = 0
countDigitsNumber a n
    | mod a 10 == n = 1 + countDigitsNumber (div a 10) n
    | otherwise = countDigitsNumber (div a 10) n
    
--sum of the digits say its 343 -> 10
sumOfDigits :: Integral p => p -> p
sumOfDigits 0 = 0
sumOfDigits n = n `mod` 10 + sumOfDigits (n `div` 10)

countDigits :: (Integral t, Integral a) => a -> a -> a -> t
countDigits n a b = countDigitsHelper a b 0
    where 
    countDigitsHelper a b sum
        | a == (b+1) = sumOfDigits sum
        | a > b  = error "Error, Not a valid input!"
        | otherwise = countDigitsHelper (a+1) b (countDigitsNumber a n + sum)

sumCountsIter :: Int -> Int -> Int
sumCountsIter x d = if d >= 0 && d <= 9 then countDigits d 1 x else error "Error, Not a valid input!"
