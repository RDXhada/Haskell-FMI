import Data.List
main :: IO ()
main = do
    print $ "hello there"
    print $ bounds [3, 7, 5, 6, 9]
    print $ bounds [3, 4, 5, 6, -1] 


bounds :: [Int] -> (Int, Int)
bounds list = if (head $ sort list < 0) then  head $ zip [1] (((sort $ list) !! 1):[]) else  head $ zip [1] (sort $ list)
