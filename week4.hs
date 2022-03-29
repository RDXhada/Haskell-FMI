main :: IO()
main = do
 print $ length1 [1, 2, 3, 4, 5, 6]

length1 :: [a] -> Int
length1 [] = 0
length1 xs = 1 + length1 (tail xs)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs)
    | n == x = True
    | otherwise = elem' n xs

null' :: [a] -> Bool
null' xs = length1 xs == 0

reverse' :: [a] -> [a]
reverse' xs
    | null' xs = []
    | otherwise = last xs : reverse' (init xs)

-- Write function getLastElement returning the last element in a list
getLastElement xs = xs !! (length1 xs - 1)

getLastElement' [] = error "Empty List"
getLastElement' [x] = x
getLastElement' (x:xs) = getLastElement' xs

-- Write function maximum1 returning the biggest element in a list 

maximum' :: Ord a => [a] -> a
maximum' [] = error "Empty list!"
maximum' [x] = x
maximum' (x:xs) = acc x xs
    where
        acc n [] = n
        acc n (x:xs)
            | n < x = acc x xs
            | otherwise = acc n xs

-- Write function getLastButOneElement returning the element before the last

getLastButOneElement :: [a] -> a
getLastButOneElement [] = error "Empty list";
getLastButOneElement [x] = error "List must have at least 2 elements"
getLastButOneElement (x:[s]) = x
getLastButOneElement (x:xs) = getLastButOneElement xs
