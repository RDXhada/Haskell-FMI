main :: IO()
main = do
    print $ checkSequence [2, 9, 15] == True
    print $ checkSequence [11, 14, 20, 27, 31] == True
    print $ checkSequence [11, 14, 28, 27, 31] == False
    print $ checkSequence [11, 14, 14, 29, 31] == False
    -- print $ all (\ (x, y) -> y > x && div y x /= 0) $ zip [] (tail [])
    print $ removeNb 26 
    print $ removeNb 100
    print $ removeNb 101


    print $ liesOn (line (0, 0) (1, 1)) (5.5, 5.5) == True
    print $ liesOn (line (0, 0) (1, 1)) (0.5, 0) == False


    print $ factorize 152 == [2, 2, 2,19]
    print $ factorize 123 == [3, 41]
    print $ factorize 13 == [13]

    print $ (specialSum (5-) [1..10]) (> 0) == 30
    print $ (specialSum (+1) [(-5)..5]) odd == 40

--Resolve test 1 from 2020.

checkSequence :: [Int] -> Bool
checkSequence [] = True
checkSequence [a] = True
checkSequence (ai:aj:as) = aj > ai && div aj ai /= 0 && checkSequence (aj:as)



removeNb :: Int -> [(Int, Int)]
removeNb n = [ (x, y) | x <- [1 .. n], y <- [1 .. n],
                x /= y && x * y == sum [1 .. n] - x - y]


type Point = (Double, Double)

line :: Point -> Point -> (Double -> Double)
line (x1, y1) (x2, y2) = (\ x -> y1 + (x - x1) * (y2 - y1) / (x2 - x1))

liesOn :: (Double -> Double) -> (Point -> Bool)
liesOn f = (\ (x, y) -> y == f x) -- (y, f x, x)

-- Define a function that accepts a natural number 
-- greater than 1 and returns a sorted list of prime factors the product of which gives the number.
factorize :: Int -> [Int]
factorize n = helper n 2
 where
     helper :: Int -> Int -> [Int]
     helper 1 _ = []
     helper leftover d
      | mod leftover d == 0 = d : helper (div leftover d) d
      | otherwise = helper leftover (d + 1)



-- Define a function that accepts an unary function and a list of natural numbers.
--  The return value should be a function that accepts a 
-- predicate and returns the sum of the squares of the numbers which squared pass the predicate.
specialSum :: (Int -> Int) -> [Int] -> ((Int -> Bool) -> Int)
specialSum f xs = (\ p -> sum [ x * x | x <- xs, p (f x)])
