main :: IO()
main = do
    print "Exercise number 4 outputs:"
    print $ areCousins t1 2 3
    print $ areCousins t2 8 4


data BTree a = Nil | Node a (BTree a) (BTree a)

t1 :: BTree Int
t1 = Node 1 (Node 2 Nil Nil)
 (Node 3 Nil Nil)


t2 :: BTree Int
t2 = Node 5 (Node 3 (Node 4 Nil Nil)
 (Node 7 Nil Nil))
 (Node 2 (Node 9 Nil Nil)
 (Node 8 Nil Nil)) 

traverseTree Nil = []
traverseTree (Node value left right) = traverseTree left ++ [value] ++ traverseTree right

areCousins :: (Eq a) => BTree a -> a -> a -> Bool
areCousins (Node value left Nil) _ _ = False
areCousins (Node value Nil right) _ _ =  False
areCousins tree@(Node value left right) _ _ = if countDepthOfLeftNodes tree <= 2 then False else True

countDepthOfLeftNodes Nil = 0
countDepthOfLeftNodes (Node value left right) = 1 + countDepthOfLeftNodes left

