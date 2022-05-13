import Data.List
main :: IO()
main = do
    --task 1
    print $ nodes [(1, 2), (1, 3), (2, 3), (2, 4)] == [1, 2, 3, 4]
    print $ neighbours 2 [(1, 2), (1, 3), (2, 3), (2, 4)] == [3, 4]
    print $ neighbours 4 [(1, 2), (1, 3), (2, 3), (2, 4)] == []
    print $ adjacencyList [(1, 2), (1, 3), (2, 3), (2, 4)] == [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]
    --task 2 
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 2, 4] == True
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 3, 4] == False
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [2, 3] == True
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [3, 1] == False
    --task 3
    print $ Circle 6
    print $ Rectangle 5 6
    print $ Triangle 4 5 6
    print $ Cylinder 4.78 9.3
    print $ Cylinder 4 5 == Rectangle 4 5
    --task 4
    print $ myImages (\x -> x * x) (2+) [Point2D 2 2, Point2D 1 2, Point2D 3 7] == [Point2D 2 2, Point2D 3 7]
    --task 5
    print $ isRound (Circle 5) == True
    print $ isRound (Rectangle 2.5 4.5) == False
    print $ isRound (Rectangle 5.5 20.6) == False
    print $ isRound (Triangle 5.3 3.9 4.89) == False
    print $ isRound (Cylinder 20 30) == True

    print $ is2D (Circle 5) == True
    print $ is2D (Rectangle 2.5 4.5) == True
    print $ is2D (Rectangle 5.5 20.6) == True
    print $ is2D (Triangle 5.3 3.9 4.89) == True
    print $ is2D (Cylinder 20 30) == False


--task 1
type Node = Int
type Edge = (Node, Node)
type Graph =[Edge]

adjacencyList :: Graph -> [(Node, [Node])]
adjacencyList g = [ (x, neighbours x g) | x <- nodes g ]

neighbours :: Node -> Graph -> [Node]
neighbours n g = [x2 | (x1, x2) <- g, x1 == n]
-- neighbours n g = map snd $ filter (\ (x, y) -> x == n) g

nodes :: Graph -> [Node]
nodes g = sort $ nub $ fst vector ++ snd vector
 where 
     vector = unzip g
    
nodes' :: Graph -> [Node]
nodes' = sort . nub . concatMap (\ (x, y) -> [x, y])


--task 2
type Graph1 =[(Node, [Node])]

member :: Node -> Node -> Graph1 -> Bool
member f c g = not $ null [ cs | (n, cs) <- g, n == f, elem c cs ]
-- member f c g = elem c $ head [ cs | (n, cs) <- g, n == f ]

isPath :: Graph1 -> [Node] -> Bool
isPath g ns = all (\ (f, c) -> member f c g) $ zip ns (tail ns)

--task 3
data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
 deriving (Show, Eq)


 --task 4
data Point2D a = Point2D a a
 deriving (Eq)

myImages :: (Eq a) => (a -> a) -> (a -> a) -> [Point2D a] -> [Point2D a]
myImages f g = filter (\ (Point2D x y) -> f x == g y)

--task 5

is2D :: Shape a -> Bool
is2D (Cylinder _ _) = False
is2D _ = True

isRound :: Shape a -> Bool
isRound (Cylinder _ _) = True
isRound (Circle _) = True
isRound _ = False
