import Data.Char

main :: IO()
main = do
    print $ numberBTree
    print $ charBTree

    print $ size numberBTree == 9
    print $ size charBTree == 7

    print $ sumTree numberBTree == 146
    -- print $ sumTree charBTree -- should not work

    print $ traverseDFS numberBTree == [96, 1, 12, 0, 5, 2, 4, 5, 21]
    print $ traverseDFS charBTree == "haskell"

    print $ getLevel numberBTree 2 == [1, 0, 2, 5]
    print $ getLevel charBTree 1 == "al"
    print $ getLevel charBTree 3 == []

    print $ traverseBFS numberBTree == [5,12,4,1,0,2,5,96,21]
    print $ traverseBFS charBTree == "kalhsel"

    print $ mapTree numberBTree (*2) == Node 10 (Node 24 (Node 2 (Node 192 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 4 Nil Nil) (Node 10 Nil (Node 42 Nil Nil)))
    print $ mapTree numberBTree (show) == Node "5" (Node "12" (Node "1" (Node "96" Nil Nil) Nil) (Node "0" Nil Nil)) (Node "4" (Node "2" Nil Nil) (Node "5" Nil (Node "21" Nil Nil)))
    print $ mapTree charBTree (toUpper) == Node 'K' (Node 'A' (Node 'H' Nil Nil) (Node 'S' Nil Nil)) (Node 'L' (Node 'E' Nil Nil) (Node 'L' Nil Nil))

    print $ constructMaxBTree [3, 2, 1, 6, 0, 5] == t
    
    print $ insert (Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 Nil (Node 18 Nil Nil))) 13 == (Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil)))

    print $ (numberTreeAfter == numberTreeBefore) == True

    print $ secondNumberTree == Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))


data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

mapTree :: BTree a -> (a -> b) -> BTree b
mapTree Nil _ = Nil
mapTree (Node value left right) f = Node (f value) (mapTree left f) (mapTree right f)

traverseBFS :: (Eq a) => BTree a -> [a]
traverseBFS t = concat $ takeWhile (/= []) $ map (getLevel t) [0 .. ]

getLevel :: BTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node value left right) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

traverseDFS :: BTree a -> [a]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

sumTree :: (Num a) => BTree a -> a
sumTree Nil = 0
sumTree (Node value left right) = value + sumTree left + sumTree right

size :: BTree a -> Int
size Nil = 0
size (Node _ left right) = 1 + size left + size right

-- Node value='k' left=(Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) right=(Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))
-- Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)
-- Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil)

numberBTree :: BTree Int
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))

charBTree :: BTree Char
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))

t :: BTree Int
t = Node 6 (Node 3 Nil (Node 2 Nil (Node 1 Nil Nil))) (Node 5 (Node 0 Nil Nil) Nil)

constructMaxBTree :: (Ord a) => [a] -> BTree a
constructMaxBTree [] = Nil
constructMaxBTree xs = Node m (constructMaxBTree $ takeWhile (/= m) xs)
                                (constructMaxBTree $ tail $ dropWhile (/= m) xs)
 where
     m = maximum xs


numberTreeBefore :: BTree Int
numberTreeBefore = Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 Nil (Node 18 Nil Nil))

numberTreeAfter :: BTree Int
numberTreeAfter = foldl insert Nil [10, 5, 15, 3, 7, 18]

secondNumberTree :: BTree Int
secondNumberTree = foldl insert Nil [10, 5, 15, 3, 7, 13, 18, 1, 6]

insert :: (Ord a) => BTree a -> a -> BTree a
insert Nil k = Node k Nil Nil
insert (Node value left right) k
 | k > value = Node value left (insert right k)
 | otherwise = Node value (insert left k) right
