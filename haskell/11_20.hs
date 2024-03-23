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

-- Time complexity: 
-- Space complexity:
dropEvery2 :: [a] -> Int -> [a]
dropEvery2 xs every = helper xs every
    where 
        helper [] _ = []
        helper (y:ys) 1 = helper ys every
        helper (y:ys) count = y : helper ys (count - 1)

-- Problem 17: Split a list into two parts; the length of the first part is given.
-- Time complexity: 
-- Space complexity:
split :: (Ord a) => Int -> [a] -> ([a], [a])
split n xs = (filterWith (<=n) xs, filterWith (>n) xs)
    where
        filterWith f = map snd . filter (f . fst) . zip [1..]

-- Time complexity: 
-- Space complexity:
split2 :: (Ord a) => Int -> [a] -> ([a], [a])
split2 n = foldr helper ([], []) . zip [1..]
    where helper (i, x) acc
            | i <= n = (x : fst acc, snd acc)
            | otherwise = (fst acc, x : snd acc)

-- Time complexity: 
-- Space complexity:
split3 :: (Ord a) => Int -> [a] -> ([a], [a])
split3 n allx@(x:xs) | n > 0 = let (l, r) = split3 (n - 1) xs in (x : l, r)
split3 _ lst = ([], lst)

-- Problem 18: Extract a slice from a list.
-- Time complexity: 
-- Space complexity:
slice :: [a] -> Int -> Int -> [a]
slice xs a b = map snd $ filter isInRange $ zip [1..] xs 
    where
        isInRange (i, _) = i >= a && i <= b

-- Time complexity: 
-- Space complexity:
slice2 :: [a] -> Int -> Int -> [a]
slice2 [] _ _ = []
slice2 _ _ 0 = []
slice2 (x:xs) 1 b =  x : slice2 xs 1 (b-1) 
slice2 (x:xs) a b = slice2 xs (a-1) (b-1)

-- Problem 19: Rotate a list N places to the left.
-- Time complexity: 
-- Space complexity:
rotate :: [a] -> Int -> [a]
rotate lst n | n >= 0 = drop n lst ++ take n lst 
rotate lst n = drop (length lst + n) lst ++ take (length lst + n) lst 

-- Problem 20: Remove the K'th element from a list.
-- Time complexity: 
-- Space complexity:
remove :: Int -> [a] -> (Maybe a, [a])
remove _ [] = (Nothing, [])
remove 1 (x:xs) = (Just x, xs)
remove k (x:xs)= let (removed, lst) = remove (k-1) xs in (removed, x:lst)