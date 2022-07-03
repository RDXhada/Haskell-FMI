import Data.List
main :: IO ()
main = do
    print $ applyEveryKth (* 2) 3 [1..6]
    print $ cook [ApplePie, ApplePie, Burger, Chicken, Chicken, ApplePie]
    print $ deepestNodesSum odd t1
    print $ deepestNodesSum even t2
    print $ biggestNumber [1,5,5,3,5]
    print $ biggestNumber [1,2,3,4,5]
    print $ poly [1,2,3,4,5] 1
    print $ getIndices [3,3] 6
    print $ dominates (+4) (*2) [1..5]
    print $ dominates (+4) (*2) [1..4]
    print $ f 3
    print $ f 1
    print $ f 8
    print $ bestStudents [("Ivan Ivanov", 6.0),("Petar Petrov", 5.5),("Maria Marinova", 6.0),("Marina Petrova", 5.0)]
    print $ iterator [3,4,5] (+1)
    print $ iterator [1,2,4] (+1)
    -- print $ iterator [1,2,3] (+1)

--task 1
applyEveryKth :: (a -> a) -> Int -> [a] -> [a]
applyEveryKth f k = zipWith (\pos x-> if mod pos k == 0 then f x else x ) [1 .. ]

--task 2
speak :: String -> (Char -> String)
speak str = (\c ->foldl (\ acc (x,pos) ->  if x==c then acc ++ show pos else acc ++ [x] ) [] $zip str $ reverse [0 .. length str - 1] )

--task 3
data Food = ApplePie | Burger | Chicken
 deriving (Show , Eq)
data Weather = Sunny | Rainy
 deriving (Show , Eq)
cook :: [Food]->[Weather]
cook xs = map(\(x1,x2)->if x1 == x2 then Sunny else Rainy) $ zip xs $ tail xs

--task 4

data BTree = Empty | Node Int BTree BTree
t1 :: BTree
t1 = Node 1 (Node 2 (Node 4 (Node 7 Empty Empty) Empty) (Node 5 Empty Empty)) (Node 3 Empty (Node 6 Empty (Node 8 Empty Empty)))
t2 :: BTree
t2 = Node 1 (Node 2 (Node 4 Empty Empty) Empty) (Node 3 Empty Empty)


treeToList :: BTree -> [Int]
treeToList Empty = []
treeToList (Node root left right) = treeToList left ++ [root] ++ treeToList right

deepestNodesSum :: (Int -> Bool) -> BTree -> Int
deepestNodesSum command tree = head $ filter command $ (take 1 $ treeToList tree) ++ (drop ((len tree)-1) $ treeToList tree)

len tree = length $ treeToList tree


--task 5 
biggestNumber list = fromDigits $ reverse $ sort $ list
fromDigits xs = sum (zipWith (*) (reverse xs) (iterate (*10) 1))

--task 6
iterator xs command = and $ map(\(x1,x2)->if x2 == (head $ map (command) (x1:[])) then True else False) $ zip xs $ tail xs
--task 7
poly :: [Int] -> (Int->Int)
poly l v = sum . (map (\(a,b) -> a*(v^b))) $ zip l [0,1..]


--task 8
getIndices :: (Enum b, Num b, Num a, Eq a, Eq b) => [a] -> a -> (b, b)
getIndices l x =  head [ (a,b) | (c, a) <- zip l [0,1..], (d, b) <- zip l [0,1..], c + d == x, a /= b]

--task 9 
dominates f g list = if (head $ reverse $ map f [1.. (head $ reverse $ list)]) >= (head $ reverse $ map g [1.. (head $ reverse $ list)]) then True else False


--task 10
f x = listToFunction [1,2,3] x
 where
    listToFunction lst x = if x <= length lst then (lst!!(x-1)) + 10 else 0

--task 11 
bestStudents :: (Ord a, Ord b) => [(a, b)] -> [a]
bestStudents lst = fst $ unzip $ zip (fst $ unzip $ sort lst) (filter (>=(head $ snd $ unzip $ sort lst)) (snd $ unzip $ sort lst))

--task 13
t3 = Node 1 (Node 12 (Node 2 Empty Empty) (Node 3 Empty Empty)) (Node 20 (Node 17 (Node 13 Empty Empty) Empty) Empty)
t4 = Node 10 (Node 2 Empty (Node 3 (Node 4 Empty Empty) Empty)) (Node 11 (Node 1 Empty Empty) (Node 6 Empty Empty))

