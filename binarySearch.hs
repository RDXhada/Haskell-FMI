import Data.List
main :: IO()
main = do
    print "hello"
    print $ isBinarySearchTree t1
    print $ isBinarySearchTree t2
    print $ isBinarySearchTree t3
    print $ sort $ treeToList t4


data BTree = Empty | Node Int BTree BTree

isBinarySearchTree :: BTree -> Bool
isBinarySearchTree tree = checkList $ treeToList tree

treeToList :: BTree -> [Int]
treeToList Empty = []         
treeToList (Node value left right) = treeToList left ++ [value] ++ treeToList right 

checkList :: (Ord a) => [a] -> Bool
checkList [] = True
checkList [x] = True
checkList (x:y:xs) = x <= y && checkList (y:xs)

t1 :: BTree 
t1 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 9 Empty Empty) (Node 14 Empty Empty))

t2 :: BTree 
t2 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 5 Empty Empty) (Node 14 Empty Empty))

t3 :: BTree
t3 = Node 8 (Node 3 (Node 5 Empty Empty) (Node 6 Empty Empty)) (Node 10 (Node 9 Empty Empty) (Node 14 Empty Empty))

t4 :: BTree 
t4 = Node 1 (Node 2 (Node 4 (Node 8 Empty Empty) (Node 9 Empty Empty)) (Node 5 (Node 10 Empty Empty) Empty)) (Node 3 (Node 6 (Node 11 Empty Empty) Empty) (Node 7 (Node 12 Empty Empty) Empty))


