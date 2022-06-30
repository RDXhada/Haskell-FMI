main :: IO()
main = do
    print $ getLevel t1 2 -- == False
    print $ grandchildrenIncreased' t1 == False
    print $ grandchildrenIncreased' t2 == True
    print $ isSymmetric Empty == True
    print $ isSymmetric t3 == False
    print $ isSymmetric t4 == True
    print $ isSymmetric t5 == True
    print $ flatten (List []) == []
    print $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1,2,3,4,5]
    print $ flatten (Elem 1) == [1]

data BTree = Empty | Node Int BTree BTree

t1 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 9 Empty Empty) (Node 14 Empty Empty))
t2 = Node 8 (Node 3 (Node 9 Empty Empty) (Node 10 Empty Empty)) (Node 10 (Node 11 Empty Empty) (Node 14 Empty Empty))

grandchildrenIncreased :: BTree -> Bool
grandchildrenIncreased Empty = True
grandchildrenIncreased t@(Node value l r)
 | null cs = True
 | otherwise = minimum cs > value && grandchildrenIncreased l && grandchildrenIncreased r
 where
     cs = getLevel t 2

grandchildrenIncreased' :: BTree -> Bool
grandchildrenIncreased' Empty = True
grandchildrenIncreased' (Node value l r) = null cs || minimum cs > value && grandchildrenIncreased l && grandchildrenIncreased r
 where
     cs = getLevel (Node value l r) 2

getLevel :: BTree -> Int -> [Int]
getLevel Empty _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node _ left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

t3 :: BTree                         --   1
t3 = Node 1 (Node 2 Empty Empty)    --  / \
            (Node 3 Empty Empty)    -- 2   3

t4 :: BTree                                 --     1
t4 = Node 1 (Node 2 (Node 3 Empty Empty)    --    / \
                    Empty)                  --   2   2
            (Node 2  Empty                 --  /     \
                    (Node 3 Empty Empty))   -- 3       3

t5 :: BTree                                         --       1
t5 = Node 1 (Node 2 (Node 3 Empty Empty)            --    /     \
                    (Node 7 (Node 5 Empty Empty)    --   2       2
                            Empty))                 --  / \     / \
            (Node 2 (Node 7 Empty                   -- 3   7   7   3
                            (Node 5 Empty Empty))   --    /     \
                    (Node 3 Empty Empty))           --   5       5

isSymmetric :: BTree -> Bool
isSymmetric Empty = True
isSymmetric (Node value left right) = traverseDFS left == (reverse $ traverseDFS right)

traverseDFS :: BTree -> [Int]
traverseDFS Empty = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right



data NestedList = Elem Int | List [NestedList]
flatten :: NestedList -> [Int]
flatten (Elem value) = [value]
flatten (List cs) = concatMap flatten cs
