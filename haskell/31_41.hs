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


main :: IO ()
main = do
    print $ myGCD 12 8