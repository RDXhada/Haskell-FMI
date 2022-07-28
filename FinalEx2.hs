main :: IO()
main = do
    print $ prodEvens [1,2,3,4,5,6] 
    print $ prodEvens [7.66,7,7.99,7] 
    print $ prodEvens [2,2,3,5,6]


prodEvens :: (Num a) => [a] -> a
prodEvens list = product $ evenOnly list

evenOnly :: [a] -> [a]
evenOnly xs = let g = iterate (\x -> (x + 1) `mod` 2) 0
  in
  foldr (\ (x, n) s -> if n == 0 then x : s else s) [] (zip xs g)

