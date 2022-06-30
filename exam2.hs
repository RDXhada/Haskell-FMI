import Data.List
main :: IO ()
main = do
    print "Task 1:"
    print $ filterTypical ["Mallard", "Hook Bill", "African", "Crested", "Pilgrim", "Toulouse", "Blue Swedish"] == ["Mallard", "Hook Bill", "Crested", "Blue Swedish"]
    print $ filterTypical ["Mallard", "Barbary", "Hook Bill", "Blue Swedish", "Crested"] == ["Mallard", "Barbary", "Hook Bill", "Blue Swedish", "Crested"]
    print $ filterTypical ["African", "Roman Tufted", "Toulouse", "Pilgrim", "Steinbacher"] == []
    print $ "Task 2:"
    print $ cP [Present, Absent, Absent] == False
    print $ cP [Present, Late, Present, Absent, Present, Present, Present, Absent] == False
    print $ cP [Present, Late, Present, Late, Present, Late, Present, Absent, Late, Present] == True
    print $ cP [Present, Late, Present, Late, Late, Late, Present, Present, Absent, Present] == False
    print $ "Task 3:"
    print $ maxSumSubT t1 == 5
    print $ maxSumSubT t2 == 2
    print $ "Task 4:"
    print $ isBoring t1 == False
    print $ isBoring t2 == True
 

--task 1
filterTypical :: [String] -> [String]
filterTypical nt = filter (\x -> not (x `elem` typical)) nt

typical :: [[Char]]
typical = ["African", "Roman Tufted", "Toulouse", "Pilgrim", "Steinbacher"]

--task 2
cP :: StudentRecord -> Bool
cP = canPass (1,2)

canPass :: Criterion -> (StudentRecord -> Bool)
canPass (n, k) = (\ studrec -> canPass' studrec 0 0)
 where
  canPass' [] absent late = absent <= n && late <= k 
  canPass' (r:rec) absCount lateCount
    | absCount > n || lateCount > k = False 
    | r == Absent = canPass' rec (absCount + 1) 0
    | r == Late =   canPass' rec (absCount) (lateCount + 1)
    | otherwise =   canPass' rec (absCount) 0
 
type Misses = Int
type Lates = Int
type Criterion = (Misses, Lates)
data Attendance = Absent | Late | Present
 deriving(Eq)
type StudentRecord = [Attendance]

--task 3
data BTree a = NullT | Node a (BTree a) (BTree a)

t1:: BTree Int
t1 = Node 3 (Node 0 NullT NullT) (Node 2 (Node 0 NullT NullT) NullT)

t2:: BTree Int
t2 = Node (-3) (Node 0 NullT NullT) (Node 2 (Node 0 NullT NullT) NullT)


maxSumSubT :: (Ord a, Num a) => BTree a -> a
maxSumSubT tree = sum $ filter (>0) $ sort $ traverseDFS tree

--make tree into list
traverseDFS :: BTree a -> [a]
traverseDFS NullT = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

-- maximum' :: Ord a => [a] -> a
-- maximum' = foldr1 (\x y ->if x >= y then x else y)

--task 4
data NTree a = NullT | Node a [(NTree a)]

isBoring :: (Eq a) => NTree a -> Bool
isBoring NullT = True
isBoring (Node _ []) = True
isBoring (Node x sucs) = length sucs == (length $ takeWhile (\ suc@(Node y _) -> x == y && isBoring suc) sucs)

t1 :: NTree Int
t1 = Node 10 [(Node 10 [(Node 10 []), (Node 8 [(Node 10 [])]), (Node 2 [])]), (Node 10 [(Node 11 []), (Node 10 []), (Node 6 [])])]
t2 :: NTree Char
t2 = Node 's' [(Node 's' []), (Node 's' []), (Node 's' [])]
 
