main :: IO ()
main = do
    print $ "he"
    print $ findNb 1071225 
    print $ findNb 40539911473216
    print $ findNb 135440716410000
    print $ findNb 4183059834009 
    print $ findNb 91716553919377
    print $ findNb 24723578342962

    
findNb :: Integer -> Integer
findNb n = helper n 1
 where
    helper 0 c = c - 1
    helper n c  
        | n < 0 = -1 
        | otherwise = helper (n-c^3) (c+1)


