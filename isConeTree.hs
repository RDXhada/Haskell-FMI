main :: IO()
main = do
    print "hello there"
    print $ cone t1 == "it is a cone tree"


data Tree a = Leaf a | Node (Tree a) (Tree a)

countLeftNodes :: Tree a -> Int
countLeftNodes (Leaf a) = 0
countLeftNodes (Node left right) = 1 + countLeftNodes left

countRightNodes :: Tree a -> Int
countRightNodes  (Leaf a) = 0
countRightNodes (Node left right) = 1 + countRightNodes right

t1 = Node((Node (Leaf 1) (Node (Leaf 10) (Leaf 11))))(Node (Leaf 12) (Leaf 13))

cone tree = if countLeftNodes tree == countRightNodes tree then "it is a cone tree" else "it is NOT a cone tree"
