-- fibonacci with helper
fibIteration :: (Eq t1, Num t1, Num t2) => t1 -> t2
fibIteration x = fibHelper x 0 1
    where
        fibHelper 0 _ b = b
        fibHelper n a b = fibHelper (n-1) b (a+b)

--factorial with helper
factorial :: (Eq t, Num t) => t -> t
factorial x = factorialHelper x 1
    where
        factorialHelper 1 a = a
        factorialHelper n a =  factorialHelper (n-1) (a*n)

--reverse a given number
reverseNumber n = reverseHelper n 0
    where
        reverseHelper 0 b = b
        reverseHelper a b = reverseHelper (a `div` 10) (b*10 + a `mod` 10)
        

--if the integer is symmetric
isSymmetric :: Integral t => t -> Bool
isSymmetric x = x == reverseNumber x

--Sum of digits

sumOfDigits :: Integral p => p -> p
sumOfDigits 0 = 0
sumOfDigits n = n `mod` 10 + sumOfDigits (n `div` 10)
-- Can we write this with Iter?

-- findPrimeNumbers
isPrime 1 = False 
isPrime 2 = True 
isPrime n = isPrimeHelper n 2 (n-1)
    where
        isPrimeHelper n from to
            | from == to = True
            | mod n from == 0 =  False
            | otherwise = isPrimeHelper n (from + 1) to


-- Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
-- Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer. (Problem 40)
-- goldbach 28 = 5
-- goldbach 12 = 5 // 12 = 5 + 7

{--
goldbach x = goldbachHelper x 2 x -- can we optimise this ?
    where goldbachHelper from to
        | ...
--}
-- Write function calculate resolving 3n+1 problem for a spesific Integer and finding the length of this solution
-- Write function maximumPath finding the Integer with the longest path for 3n+1 problem in interval [a, b]
-- White function isIncrease(Decrease) validating if the digits of a number increase/decrease e.g. 13456 -> True , 45623 -> False


-- Write function gcd1 finding greatest common divisor of 2 numbers
gcd1 0 b = b
gcd1 a 0 = a
gcd1 a b = gcd1 (mod b a) a

-- Write function lcm1 finding lowest common multiple of 2 numbers
-- Write function area finding area of a triangle by defined sides of the triangle
-- Write function biggest finding the bigest digit in a Int (Integer)
