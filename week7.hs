
import Data.List

main :: IO()
main = do
    print $ sumOfEvenly 1 10 == 41
    print $ sumOfEvenly 5 20 == 175

    print $ sumOfEvenly' 1 10 == 41
    print $ sumOfEvenly' 5 20 == 175
    print $ (kthMaxMin [-1]) 1 == -1
    -- print $ (kthMaxMin [-1,0,-1,0,-2,3,1,-1]) 3 -- error “No such number”
    print $ (kthMaxMin [-1,-5,-6,-6,-6,-6]) 2 == -5
    print $ (kthMaxMin [1,2,3,4,-5,6,7,-2,-1,0]) 2 == -2
    -- print $ nub [ x | x <- [-1,-5,-6,-6,-6,-6], x < 0]
    -- print $ reverse $ sort $ nub $ filter (< 0) [1,2,3,4,-5,6,7,-2,-1,0]

    print $ persistence 39  == (3,[27,14,4])      --3*9=27, 2*7=14, 1*4=4
    print $ persistence 999 == (4,[729,126,12,2]) --9*9*9=729, 7*2*9=126,  
    print $ persistence 126 == (2,[12,2])         --1*2*6=12, 1*2=2
    print $ persistence 4   == (1,[4])

    print $ scoreHand ["4","5","6"] == 15
    print $ scoreHand ["7","7","8"] == 22
    print $ scoreHand ["K","J","Q"] == 30

--TEST 1
sumOfEvenly' :: Int -> Int -> Int
sumOfEvenly' a b = sum $ filter (\ x -> even $ countOfDiv' x) [a .. b]

countOfDiv' :: Int -> Int
countOfDiv' n = length $ filter (\ x -> mod n x == 0) [1 .. n]

sumOfEvenly :: Int -> Int -> Int
sumOfEvenly a b
 | a > b = 0
 | even (countOfDiv a)= a + sumOfEvenly(a+1) b
 | otherwise = sumOfEvenly (a+1)b

countOfDiv:: Int-> Int
countOfDiv n = helper 1 0
 where 
     helper :: Int->Int -> Int
     helper i count 
      |i >= n = (count+1)
      |mod n i == 0 = helper (i+1)(count+1)
      |otherwise =helper (i+1)(count)

--TASK2

kthMaxMin :: [Int] -> (Int -> Int)
kthMaxMin xs = (\ k -> if length (getSortedLst' xs) < k
                        then error "No such number"
                        else (getSortedLst' xs)!!(k-1))
 where
    getSortedLst = reverse $ sort $ nub $ filter (< 0) xs

-- functional level
getSortedLst' = reverse . sort . nub . filter (< 0)

--TASK 3
prodDigits :: Int -> Int
prodDigits n = helper 1 n
 where
     helper :: Int -> Int -> Int
     helper result leftover
      | leftover < 10 = result * leftover
      | otherwise = helper (result * mod leftover 10) (div leftover 10)

persistence :: Int -> (Int, [Int])
persistence n = (length $ ys n, ys n)
 where
     ys :: Int -> [Int]
     ys leftover
      | prodDigits leftover < 10 = [prodDigits leftover]
      | otherwise = (prodDigits leftover) : ys (prodDigits leftover)



--TASK 4
helper card
 | elem card ["J", "Q", "K"] = 10
 | otherwise = read card

score currentSum [] = currentSum
score currentSum aces
 | currentSum + 11 * (length aces) > 21 = score (currentSum + 1) (tail aces)
 | otherwise = score (currentSum + 11 * (length aces)) []

scoreHand :: [String] -> Int
scoreHand xs = let parts = partition (/= "A") xs in
        score (sum $ map helper $ fst parts) (snd parts)
