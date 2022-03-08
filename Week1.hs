 {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
main::IO ()
main = do
    print $ doubleMe 4
    print $ doubleUs 4 5
    print $ dummy 6
    print $ d 3 4 5
    print $ max1 3 5
    print $ max2 3 5
    print $ guessTheNumber 9
    print $ fact1 5
    print $ plus 3 4
    print $ fib1 7
    print $ smallNumber 1000
    print $ bigNumber 99
    print $ factorial1 6
    print $ factorial2 7
    print $ doubleMe 4

doubleMe x = x + x  
doubleUs :: Num a => a -> a -> a
doubleUs x y = x*2 + y*2   
dummy :: p -> p
dummy x = x

--discriminanta
d :: Num a => a -> a -> a -> a
d a b c = b*b - 4*a*c

max1 :: Ord p => p -> p -> p
max1 a b = if a > b then a else b
--with guards 
max2 :: Ord p => p -> p -> p
max2 a b
    | a > b = a
    | otherwise = b

guessTheNumber :: (Eq a, Num a) => a -> [Char]
guessTheNumber 10 = "Correct One!"
guessTheNumber x = "Wrong number bro!"

--factorial 
fact1 :: (Eq p, Num p) => p -> p
fact1 0 = 1
fact1 1 = 1
fact1 x = x * fact1 (x - 1)

--sum of two numbers
plus :: Num a => a -> a -> a
plus a b = a + b

--fibonacci sequence
fib1 :: (Eq a, Num a, Num p) => a -> p
fib1 0 = 0
fib1 1 = 1
fib1 x = fib1(x-1) + fib1(x-2)


smallNumber :: (Ord a, Num a) => a -> Bool
smallNumber x = if x > 10
    then  True
    else  False

bigNumber :: (Ord a, Num a ) => a -> Bool
bigNumber x = if x > 100
    then True 
    else False

--factorial
factorial1 :: (Eq p, Num p) => p -> p
factorial1 0 = 1
factorial1 1 = 1
factorial1 x = x * factorial1(x - 1)

--2nd way factorial with guards
factorial2 :: Int -> Int
factorial2 x
    | x == 0 = 1
    | otherwise = x * factorial2(x - 1)
