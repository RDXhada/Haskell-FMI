-- transpose matrix defined as list of lists
-- [[1,2],[3,4]] => [[1,3], [2,4]]
transpose1 :: [[a]] -> [[a]]
transpose1 ([]:_) = []
transpose1 l = map head l : transpose1 (map tail l)

-- Validate if for one number x all function will be applicable, return true
-- validateNumber 1 [(\x -> x < 4), (\x -> x*x == x)] == True

validateNumber1 num l = [f | f <- l, f num == True]
validateNumber2 num l = length l == length (validateNumber1 num l) 

validateNumber3 num l = map (\x -> x num)  l 
validateNumber4 num l = foldr1 (&&) (validateNumber3 num l)


-- Implement collatzConjecture n return [Integer] generating https://en.wikipedia.org/wiki/Collatz_conjecture with a list of all numbers for this rule from n up to 1
-- collatzConjecture 1 == [1]
-- collatzConjecture 3 == [3, 10, 5, 16, 8, 4 , 2, 1]

collatzConjecture n
    | n == 1 = [1]
    | n `mod` 2 == 0 = n : collatzConjecture ( n `div` 2)
    | otherwise = n : collatzConjecture( 3 * n + 1) 


-- We have [(City, Temperaure)] defined by [(Integer, [Char])]. Find the name of the city most close up to the average one into the list

l :: [(Float, [Char])]
l = [(16, "Sofia"),(30, "Burgas"), (26, "Varna")]

-- citySum l = sum $ map (\(a,b) -> a) l 
-- aveaverageTemprage xs = foldr (\x xs -> x : xs) []
averageTemp :: (Fractional a1, Real a2) => [(a2, b)] -> a1
averageTemp l = realToFrac (sum $ map (\(a,b) -> a) l) /  fromIntegral (length l) -- to have working code, definition of l should be l [(Float, [Char])] and not [(Integer, [Char])] 
--closeToAverageTemp :: Fractional a => [(a, b)] -> [(a, b)]
closeToAverageTemp :: (Fractional a, Real a) => [(a, b)] -> [(a, b)]
closeToAverageTemp  l = [ (abs(a - averageTemp l ), b ) | (a,b) <- l ]
findCity :: (Fractional a, Real a) => [(a, b)] -> b
findCity l = snd $ foldl1 (\(a,b) (a1, b1) -> if a > a1 then (a1,b1) else (a,b)) (closeToAverageTemp  l) 

-- Implement quicksort1
-- Implement maxCollatzConjecture a b, return the number with the longest path from a up to b.

{--
Напишете ф-я lSystem axiom rules, която да връща безкраен списък от стрингове със следните свойства:
axiom е стринг.
rules е списък от наредени двойки (Char, [Char])
Всяки следващ стринг трябва да е получен чрез заместване на символите от предния стринг със стрингове от правилата.
Първият стринг в безкрайният списък от състояния е аксиомата.
Пример:

axiom = 'A'
rules = [('A', "AB"), ('B', "A")]
last $ take 7 $ lSystem axiom rules
> ABAABABAABAABABAABABAABAABABAABAAB
The task is from fmi-fp-2020-21
--}
