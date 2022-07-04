main :: IO()
main = do
    print $ traverse1 t1
data BTree = Empty | Node Int BTree BTree
t4 :: BTree 
t4 = Node 1 (Node 2 (Node 4 (Node 8 Empty Empty) (Node 9 Empty Empty)) (Node 5 (Node 10 Empty Empty) Empty)) (Node 3 (Node 6 (Node 11 Empty Empty) Empty) (Node 7 (Node 12 Empty Empty) Empty))
t1 :: BTree
t1 = Node 1 (Node 12 (Node 2 Empty Empty) (Node 3 Empty Empty)) (Node 20 (Node 17 (Node 13 Empty Empty) Empty) Empty)

traverse1 Empty = []
traverse1 node@(Node value l r) = traverse1 l ++ helper node ++ traverse1 r
    where helper node = if isCorrect node == True then [getValue node] else [] 

getValue Empty = error "No value for Empty tree"
getValue (Node value _ _) = value

isCorrect Empty = False
isCorrect (Node _ Empty _) = False
isCorrect (Node _  _ Empty) = False
isCorrect (Node value (Node v1 _ _) (Node v2 _ _)) = value > (v1+v2)
