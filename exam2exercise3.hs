import Data.List
main :: IO ()
main = do
    print $ "task 3 outputs: "
    print $ maxSumSubT t1 == 5
    print $ maxSumSubT t2 == 2
 

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


