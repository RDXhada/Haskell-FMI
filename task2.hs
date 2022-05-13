main :: IO()
main = do
    print $ maxRotation 56789
    --return true if its correct
    print $ maxRotation 56789 == 68957
    print $ maxRotation 12490
    print $ maxRotation 123
    print $ maxRotation 67
    --returns empty list error: 
    --print $ maxRotation 0
    print $ maxRotation (-1234)

--function to help rotateDigits
rotationHelper :: [a] -> [a]
rotationHelper [] = []
rotationHelper (x:xs) = xs ++ [x]

--function show converts its argument(say int) to a string
--read casts strings to another type(say int)
rotateDigits :: Int -> Int -> Int
rotateDigits x y
  | y == 0 = (read . rotationHelper . show) x
  | otherwise = read $ rotationHelper $ drop y $ show x

--function to find the number of digits in a given number
--turns the number into a list and finds the length, hence the max counter
digitsList :: Integral a => a -> [a]
digitsList 0 = []
digitsList x =  digitsList (x `div` 10) ++ [x `mod` 10]
--count of digits
digitsCount :: Integral a => a -> Int
digitsCount x = if x < 0 then error "Error, must be a positive number!" else length (digitsList x)

--function to find the max of a given number in a list
maximumNum :: Ord a => [a] -> a
maximumNum = foldr1 (\x y ->if x >= y then x else y)

--maxRotation function
-- we write drop 3 [1,2,3,4,5] returns [4,5] (drop function)
-- read turns the strings to another argument (say int)
maxRotation :: Int -> Int
maxRotation x = maximumNum $ maxHelper x 0
  where
      maxHelper y z = if z >= digitsCount x then [] else result : maxHelper result (z + 1)
          where
              result = read $ take z (show y) ++ rotationHelper (drop z (show y))
