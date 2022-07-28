main :: IO()
main = do 
    print $ primesProd 12 == 6
    print $ primesProd 1200 

--generates the prime numbers
primeGenerator :: Int -> [Int]
primeGenerator 1 = []
primeGenerator n = if isPrime then n:numbersList else numbersList
    where numbersList = primeGenerator (n-1)
          isPrime = all (\d -> n `mod` d /= 0) numbersList

primesProd :: Int -> Int
primesProd x = product (primeGenerator sqNum)
    where
        sqNum = floor (sqrt(fromIntegral x))
