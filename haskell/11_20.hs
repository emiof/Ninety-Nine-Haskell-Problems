packConsecutives :: (Eq a) => [a] -> [[a]]
packConsecutives = foldr pack []
    where
        pack x ally@(y:ys) = if x == head y then (x:y):ys else [x]:ally
        pack x [] = [[x]]

-- Problem 11: Modified run-length encoding.
-- Time complexity: 
-- Space complexity:
data Coded a = Single a | Multiple Int a deriving Show
encodeModified :: (Eq a) => [a] -> [Coded a]
encodeModified xs = [if l >= 1 then Multiple (l + 1) y else Single y | (y:ys) <- packConsecutives xs, let l = length ys]

-- Problem 12: Decode a run-length encoded list.
-- Time complexity: 
-- Space complexity:
decodeModified :: [Coded a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decode x ++ decodeModified xs
    where 
        decode (Single s) = [s]
        decode (Multiple n m) = [m | _ <- [1..n]]

-- Time complexity: 
-- Space complexity:
decodeModified2 :: [Coded a] -> [a]
decodeModified2 = concatMap decode 
    where 
        decode (Single s) = [s]
        decode (Multiple n m) = replicate n m

-- Problem 13: Run-length encoding of a list (direct solution).
-- Time complexity: 
-- Space complexity:
encodeDirect :: (Eq a) => [a] -> [Coded a]
encodeDirect = map encode . pack 
    where
        pack [] = []
        pack [x] = [[x]]
        pack (x:xs) = let ally@(allz@(z:_):ys) = pack xs in if x == z then (x:allz):ys else [x]:ally
        encode [x] = Single x
        encode allx@(x:_) = Multiple (length allx) x

encodeDirect2 :: (Eq a) => [a] -> [Coded a]
encodeDirect2 [] = []
encodeDirect2 (x:xs)
    | count == 0 = Single x : encodeDirect2 xs
    | otherwise = Multiple (count + 1) x : encodeDirect2 rest
    where
        (match, rest) = span (==x) xs
        count = length match 

-- Problem 14: Duplicate the elements of a list.
-- Time complexity: 
-- Space complexity:
duplicateElements :: [a] -> [a]
duplicateElements = concatMap (replicate 2)

-- Time complexity: 
-- Space complexity:
duplicateElements2 :: [a] -> [a]
duplicateElements2 = foldr (\x acc -> x:x:acc) []

-- Problem 15: Replicate the elements of a list a given number of times.
-- Time complexity: 
-- Space complexity:
replicateElements :: [a] -> Int -> [a]
replicateElements xs n = concatMap (replicate n) xs

-- Time complexity: 
-- Space complexity:
replicateElements2 :: [a] -> Int -> [a]
replicateElements2 [] _ = []
replicateElements2 (x:xs) n = [x | _ <- [1..n]] ++ replicateElements2 xs n

-- Problem 16: Drop every N'th element from a list.
-- Time complexity: 
-- Space complexity:
dropEvery :: [a] -> Int -> [a]
dropEvery xs every = [x | (i, x) <- zip [1..] xs, mod i every /= 0]

-------------------------
-- Problem n:
-- Time complexity: 
-- Space complexity:

main :: IO()
main = do 
    print $ dropEvery "abcdefghik" 3