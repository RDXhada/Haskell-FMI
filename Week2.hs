factorielIter :: (Eq t, Num t) => t -> t
factorielIter x = factorielHelper x 1
    where 
        factorielHelper 1 a = a
        factorielHelper n a = factorielHelper (n-1) (n*a)

-- Write function reverseInteger that reverse a number
-- reverseInteger 3892 -> 2983
-- reverseInteger 12300 -> 321
{--
How to resolve this problem
reverseInteger 0 = 0
reverseInteger x = (reverseInteger( div x 10 ) * 10) + mod x 10

reverseInteger 0 = 0
reverseInteger x = mod x 10 * 10 + reverseInteger(div x 10)
--}
reverseIter x = reverseHelper x 0
    where
        reverseHelper 0 b = b
        reverseHelper a b = reverseHelper (div a 10) (b*10 + mod a 10)
        

-- White function isSymetricInteger
-- isSymetricInteger 14541 == True
-- isSymetricInteger 123 == False
isSymetricInteger :: Integral t => t -> Bool
isSymetricInteger x = x == reverseIter x

-- Write function sumOfDigits returning the sum of the digits of a int number.
-- sumOfDigits 1234 = 10
sumOfDigits 0 = 0
sumOfDigits x = mod x 10 + sumOfDigits(div x 10)
-- Can we write this with Iter?

-- Determine whether a given integer number is prime. (Problem 31)
isPrime 1 = False
isPrime 2 = True
isPrime x = isPrimeHelper x 2 (x-1) -- can we optimise this validation?
    where
        isPrimeHelper x from to -- not neccesery so send x as a param since he is in the same scope
            | from == to = True
            | mod x from == 0 = False
            | otherwise  = isPrimeHelper x (from+1) to
-- for isPrime 2 => isPrimeHelper x 2 1

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
