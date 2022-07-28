import Data.List
main :: IO()
main = do
    print $ "task 4 outputs: "
    print $ isBoring t1 == False
    print $ isBoring t2 == True

data NTree a = NullT | Node a [(NTree a)]

isBoring :: (Eq a) => NTree a -> Bool
isBoring NullT = True
isBoring (Node _ []) = True
isBoring (Node x sucs) = length sucs == (length $ takeWhile (\ suc@(Node y _) -> x == y && isBoring suc) sucs)

t1 :: NTree Int
t1 = Node 10 [(Node 10 [(Node 10 []), (Node 8 [(Node 10 [])]), (Node 2 [])]), (Node 10 [(Node 11 []), (Node 10 []), (Node 6 [])])]
t2 :: NTree Char
t2 = Node 's' [(Node 's' []), (Node 's' []), (Node 's' [])]
