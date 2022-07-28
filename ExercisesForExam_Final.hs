import Data.Char
import Data.List

main :: IO()
main = do
    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False
    print $ isInteresting 70 == True 
    print $ isInteresting 5 == True 
    print $ isInteresting 4 == True


    print $ specialSum 1 100 == 195 -- 61, 65, 69
    print $ sum $ map digitToInt $ show 123 

    print $ isArithmentic [1, 2, 3, 4, 5]
    print $ onlyArithmentic [[3], [1, 2, 3, 4, 5], [3, 5, 8, 9, 11]]  == [[3], [1, 2, 3, 4, 5]]


    print $ mySin 100 1 == 0.8414709848078965 -- n = 100, x = 1
    print $ mySin 100 0.5 == 0.479425538604203


    print $ dominatesFold (\x -> x + 1) (\x -> x + 2) [1, 2, 3, 4, 5] == False
    print $ dominatesFold (\x -> x * 3) (\x -> x + 2) [1, 2, 3, 4, 5] == True


    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 2), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 2)] == "English"
    
    
    print $ reverseOrdSuff 37563 -- == 36
    print $ reverseOrdSuff 32763 -- == 367
    print $ reverseOrdSuff 32567 -- == 7
    print $ reverseOrdSuff 32666 -- == 6
    print $ reverseOrdSuff 32660 -- == 6

    print $ onlyUnique [1,2,3,2]
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45


    print $ getClosestDistance [(4, 5, 6), (2, 5, 10), (5, 2, (-10)), ((-2), 1, 45), (12, 0, 2), (6, 5, 4)] == 2.83


--A number is interesting if and only if 
-- it is evenly divided by the sum of its digits. 
--Define a predicate that checks whether a number is interesting.
isInteresting :: Int -> Bool
isInteresting n = mod n (sum $ map digitToInt $ show n) == 0


-- Define a function that returns 
-- the sum of the special numbers in the interval 
-- [x, y] (x <= y). A number is special if it contains 
--     6 and can be expressed as 4k + 1, where k is a whole number.
specialSum :: (Integral a, Show a) => a -> a -> a
specialSum a b = sum [ x | x <- [a .. b], mod (x - 1) 4 == 0 && (elem '6' $ show x)]


-- Write a function that, for a list xss whose elements 
-- are non-empty lists of numbers, returns a list 
-- of those elements of xss that represent an arithmetic progression.
isArithmentic :: (Eq a, Enum a) => [a] -> Bool
isArithmentic xs = length xs < 2 || xs == take (length xs) [xs!!0, xs!!1 ..]
onlyArithmentic :: (Eq a, Enum a) => [[a]] -> [[a]]
onlyArithmentic xss = [ xs | xs <- xss, isArithmentic xs]
onlyArithmentic' :: (Eq a, Enum a) => [[a]] -> [[a]]
onlyArithmentic' = filter isArithmentic



--Sinus with sum
mySin :: Integer -> Double -> Double
mySin 0 x = x
mySin n x = ((-1)^n * x^(2*n + 1)) / (fromIntegral $ product [1 .. 2*n + 1]) + mySin (n - 1) x


-- Define a function that accepts two unary 
-- functions "f" and "g" and a list of values and 
-- checks whether f dominates g. 
-- We say that one function dominates 
-- another if for every value the 
-- absolute value after applying "f" is greater 
-- than or equal to the absolute value after applying "g".
dominatesFold :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominatesFold f g = foldl (\ acc x -> f x >= g x) True

dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates f g = all (\ x -> f x >= g x)



--By using the following types define a function that accepts a list of records and returns the hardest subject.
--Data base:
type Student = String -- име на ученик
type Subject = String -- име на предмет
type Note = Double -- оценка

-- Запис за ученик, съдържащ име на ученик, учебен предмет и
-- оценката на ученика по дадения предмет.

type Record = (Student, Subject, Note)

hardestSubject :: [Record] -> Subject
hardestSubject db = foldr1 (\ x y -> if getAvg x > getAvg y then y else x) $ nub [ n | (_, n, _) <- db]
 where
     getAvg subjName = sum grades / (fromIntegral $ length grades)
      where
          grades = [ gr | (_, n, gr) <- db, n == subjName]


-- Define a function that takes a whole number and returns its ascending left suffix. The ascending 
-- left suffix of a number is the number that forms a strictly ascending sequence, 
-- if read from right to left.

reverseOrdSuff :: Int -> Int
reverseOrdSuff n = helper (div n 10) (mod n 10)
 where
     helper :: Int -> Int -> Int
     helper 0 result = result
     helper leftover result
      | mod leftover 10 < div (mod leftover 100) 10 = helper (div leftover 10) (result * 10 + mod leftover 10)
      | otherwise = result

--Write a function that sums the unique numbers in the sublists of a list.
onlyUnique :: [Int] -> Int
onlyUnique = sum . concat . filter ((==1) . length) . group . sort

sumUnique :: [[Int]] -> Int
sumUnique = sum . map onlyUnique


--task 10
-- Define a function that accepts a list 
-- of three-dimensional points and returns the closest distance between any two points.

dist (x1, x2, x3) (y1, y2, y3) = sqrt $ (x1 - y1)**2 + (x2 - y2)**2 + (x3 - y3)**2 

roundTwoDig = (/ 100) . fromIntegral . round . (*100)

getClosestDistance :: [(Double, Double, Double)] -> Double
getClosestDistance xs = roundTwoDig $ minimum $ map (\ [x, y] -> dist x y) $ filter ((==2) . length) $ subsequences xs
