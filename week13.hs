import Data.List
main :: IO()
main = do
    --task 1
    print $ containsWord t1 "cd" == True
    print $ containsWord t1 "af" == False
    print $ containsWord t1 "ac" == False
    print $ containsWord t1 "acdh" == False
    print $ containsWord t1 "b" == False
    print $ containsWord t1 "e" == True
    print $ containsWord t2 "ab" == True
    print $ containsWord t2 "ad" == False
    print $ containsWord t3 "bdh" == True
    print $ containsWord t3 "bdi" == True
    print $ containsWord t3 "ac" == False
    --task 2
    print $ genWords t1 == ["abe","acd","acf","be","cd","cf","d","e","f"]
    print $ genWords t2 == ["ab","acd","b","cd","d"]
    print $ genWords t3 == ["abdh","abdi","abe","acf","acg","bdh","bdi","be","cf","cg","dh","di","e","f","g","h","i"]
    print $ allContain [t1, t2] == ["acd","cd","d"]
    --task 3
    print $ allContain [t1, t2, t3] == []
    print $ allContain [t3, t4] == ["g"]


data BTree a = Nil | Node a (BTree a) (BTree a)

t1 :: BTree Char
t1 = Node 'a' (Node 'c' (Node 'f' Nil Nil) (Node 'd' Nil Nil)) (Node 'b' Nil (Node 'e' Nil Nil))

t2 :: BTree Char
t2 = Node 'a' (Node 'c' (Node 'd' Nil Nil) Nil) (Node 'b' Nil Nil)

t3 :: BTree Char
t3 = Node 'a' (Node 'b' (Node 'd' (Node 'h' Nil Nil) (Node 'i' Nil Nil)) (Node 'e' Nil Nil)) (Node 'c' (Node 'f' Nil Nil) (Node 'g' Nil Nil)) 

t4 :: BTree Char
t4 = Node 'a' (Node 'c' (Node 'd' Nil Nil) Nil) (Node 'g' Nil Nil)
--Define a function that checks whether a word is present in a binary tree made up of characters.
containsWord :: (Eq a) => BTree a -> [a] -> Bool
containsWord Nil _ = False
containsWord _ [] = False
containsWord (Node c Nil Nil) [x] = c == x
containsWord (Node value left right) w@(x:xs)
 | value == x = helper left xs || helper right xs
 | otherwise = containsWord left w || containsWord right w
 where
     helper (Node value Nil Nil) [x] = value == x
     helper (Node value left right) (x:xs) = value == x && (helper left xs || helper right xs)
     helper _ _ = False

--Define a function that returns all possible word combinations from a binary tree made up of characters.
traverseDFS :: BTree a -> [a]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

genWords :: (Eq a, Ord a) => BTree a -> [[a]]
genWords t = sort $ filter (\ candidate -> containsWord t candidate) $ subsequences $ sort $ traverseDFS t

--Define a function that returns all words contained in all of the trees passed as a list.
allContain :: (Eq a, Ord a) => [BTree a] -> [[a]]
allContain = foldl1 intersect . map genWords
