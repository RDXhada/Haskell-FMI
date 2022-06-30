import Data.List
main :: IO()
main = do
  print $ toBST t1 == t1Result
  print $ toBST t2 == t2Result
  print $ toBST t3 == t3Result

--main function
toBST :: Ord a => BTree a -> BTree a
toBST Empty = Empty
toBST tree = helper tree (vector tree)
  where
    helper Empty _ = Empty
    helper (Node value left right) (x,y)
      | value == x = Node y (helper left (x,y)) (helper right (x,y))
      | value == y = Node x (helper left (x,y)) (helper right (x,y))
      | otherwise = Node value (helper left (x,y)) (helper right (x,y))

--turn the tree into an unsorted list
traverseDFS :: BTree a -> [a]
traverseDFS Empty = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right
--zip our unsorted tree with thre sorted tree
zippingFunction :: (Ord a) => BTree a -> [(a, a)]
zippingFunction tree = filter (\(x,y) -> x /= y) $ zip (traverseDFS tree ) (sort $ traverseDFS tree)

--take the head of the list of vectors
takeHead :: Ord a => BTree a -> (a, a)
takeHead tree = head $ zippingFunction tree
--take the vector
vector :: Ord a => BTree a -> (a, a)
vector = takeHead

data BTree a = Empty | Node a (BTree a) (BTree a)
  deriving (Eq, Show)
--test cases
t1 :: BTree Int
t1 = Node 2 (Node 5 Empty Empty) (Node 7 (Node 1 Empty Empty) (Node 8 Empty Empty))

t2 :: BTree Int
t2 = Node 0 (Node 1 Empty Empty) Empty 

t3 :: BTree Int
t3 = Node 5 (Node 2 (Node 1 Empty Empty) (Node 8 Empty Empty)) (Node 3 (Node 7 Empty Empty) (Node 10 Empty Empty))

t1Result :: BTree Int
t1Result = Node 2 (Node 1 Empty Empty) (Node 7 (Node 5 Empty Empty) (Node 8 Empty Empty))

t2Result :: BTree Int
t2Result = Node 1 (Node 0 Empty Empty) Empty 

t3Result :: BTree Int
t3Result = Node 5 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 8 (Node 7 Empty Empty) (Node 10 Empty Empty))





