-- Problem 31: Determine whether a given integer number is prime.
-- Time complexity: 
-- Space complexity:
isPrime :: Int -> Bool
isPrime n = let half = n `div` 2 in notElem 0 $ map (n `mod`) [half, (half - 1) .. 2]

-- Problem 32: Determine the greatest common divisor of two positive integer numbers.
-- Time complexity: 
-- Space complexity:
myGCD :: Int -> Int -> Int
myGCD a 0 = a 
myGCD a b = myGCD b (a `mod` b)

-- Problem 33: Determine whether two positive integer numbers are coprime.
-- Time complexity: 
-- Space complexity:
coprime :: Int -> Int -> Bool
coprime = curry ((==1) . uncurry myGCD)

-- Problem 34: Calculate Euler's totient function phi(m).
-- Time complexity: 
-- Space complexity:
totient :: Int -> Int 
totient n = length $ filter (coprime n) [1..n]

-- Time complexity: 
-- Space complexity:
totient2 :: Int -> Int 
totient2 n = helper n 0
    where
        helper :: Int -> Int -> Int
        helper 0 sum = sum
        helper curr sum = helper (curr-1) (sum + if coprime n curr then 1 else 0)

-- Problem 35: Determine the prime factors of a given positive integer.
-- Time complexity: 
-- Space complexity:
primeFactors :: Int -> [Int]
primeFactors = filter isPrime . enumFromTo 1


main :: IO ()
main = do
    print $ primeFactors 315