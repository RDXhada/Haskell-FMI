
sumNumbers 1 = 1
sumNumbers n = n + sumNumbers (n-1)

sumNumbers1 n = div (n * (n+1)) 2

summation n 1 = sumNumbers1 n
summation n m = summation (summation n (m-1)) 1

{-- 
Write function calculateF
calculateF(n) = n, if n < 3
calculateF(n) = calculateF(n - 1) + 2*calculateF(n - 2) + calculateF(n - 3), otherwise
Implement also iterative solution
--}

calculateF n = if n < 3 then n else calculateF(n - 1) + 2*calculateF(n - 2) + calculateF(n - 3)

calculateF1 n
    | n < 3 = n
    | otherwise = calculateF1 (n - 1) + 2 * calculateF1(n - 2) + calculateF1(n - 3)

-- Write function coundDigits n a b ,calculating how many time there is the digit n in range of Integers [a,b]
coundDigitsNumber 0 _ = 0
coundDigitsNumber a n
    | mod a 10 == n = 1 + coundDigitsNumber (div a 10) n
    | otherwise = coundDigitsNumber (div a 10) n

coundDigits n a b = coundDigitsHelper a b 0
    where 
    coundDigitsHelper a b sum
        | a == (b+1) = sum
        | a > b = error "Not a valid input"
        | otherwise = coundDigitsHelper (a+1) b (coundDigitsNumber a n + sum) 

-- coundDigits 1 11 14 => 5
-- Write function calculateSin n by the formula in week03-task.png

-- fact1 :: (Eq p, Num p) => p -> p
fact1 1 = 1
fact1 x = fact1 (x-1) * x


--calcTeylor :: Double -> Int -> Double
--calcTeylor x k = ((-1) ^ k) * (x ^ (2*k+1)) / fact1 (2*k - 1)
-- ^ vs ** vs ^^
--calculateTeylor :: (Floating p, Eq p) => p -> p -> p
calculateTaylor :: (Floating p, Eq p) => p -> p -> p
calculateTaylor x n = (((-1) ** n) * (x ** (2*n+1))) / fact1(2*n+1)

calculateSin n x = calculateSinIter 1 n 0
    where
    calculateSinIter begin end sum
        | begin == (end + 1) = sum
        | otherwise = calculateSinIter (begin + 1) end (sum + calculateTaylor x n)


{--
Calculate the numbers of all n-digit strictly increasing numbers in the interval [a,b]
[0,9] -> 10
0 , 1,2...
134 -> correct one
132 -> 3 > 2, 1<3 so false Here
--}
-- Write function sumOfPrimes finding the sum of all prime numbers in the interval [a, b]
-- Write function canbePrime n finding if there is a way by remove one of the digits in n the number became a prime.
