-- Problem 1: Find the last element of a list.
-- Time complexity: O(n)
-- Space complexity: O(n)
lastElement :: [a] -> Maybe a
lastElement [] = Nothing 
lastElement [x] = Just x
lastElement (_:xs) = lastElement xs

-- Time complexity: O(n)
-- Space complexity: O(n)
lastElement2 :: [a] -> Maybe a 
lastElement2 = foldl (\_ x -> Just x) Nothing 

-- Problem 2: Find the last-but-one (or second-last) element of a list.
-- Time complexity: O(n)
-- Space complexity: O(n)
secondLast :: [a] -> Maybe a 
secondLast [] = Nothing
secondLast [x] = Nothing
secondLast [x, _] = Just x
secondLast (_:xs) = secondLast xs

-- Time complexity: O(n)
-- Space complexity: O(n)
secondLast2 :: [a] -> Maybe a 
secondLast2 xs =
    let res = [x | (i, x) <- zip [1..] xs, i == (length xs - 1)]
    in if null res then Nothing else Just $ head res

-- Problem 3: Find the K'th element of a list. 
-- Time complexity: O(n)
-- Space complexity: O(n)
kElement :: Int -> [a] -> Maybe a 
kElement _ [] = Nothing
kElement 0 (x:_) = Just x
kElement k (_:xs) = kElement (k - 1) xs

-- Time complexity: O(n)
-- Space complexity: O(n)
kElement2 :: Int -> [a] -> Maybe a 
kElement2 k xs = 
    let res = [x | (i, x) <- zip [0..] xs, i == k]
    in if null res then Nothing else Just $ head res

-- Problem 4: Find the number of elements in a list.
-- Time complexity: O(n)
-- Space complexity: O(n)
numElements :: [a] -> Int 
numElements [] = 0
numElements (x:xs) = 1 + numElements xs

-- Time complexity: O(n)
-- Space complexity: O(n)
numElements2 :: [a] -> Int
numElements2 = foldl (+) 0 . map (\_ -> 1)

-- Problem 5: Reverse a list.
-- Time complexity: 
-- Space complexity:
reverseElements :: [a] -> [a]
reverseElements [] = []
reverseElements (x:xs) = reverseElements xs ++ [x]

-- Time complexity: 
-- Space complexity:
reverseElements2 :: [a] -> [a]
reverseElements2 = foldl (flip (:)) []

-- Problem 6: Find out whether a list is a palindrome.
-- Time complexity: 
-- Space complexity:
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)

-- Problem 7:  Flatten a nested list structure.
-- Time complexity: 
-- Space complexity:
data NestedList a = Elem a | List [NestedList a]
flattenList :: NestedList a -> [a]
flattenList (Elem a) = [a]
flattenList (List []) = []
flattenList (List (x:xs)) = flattenList x ++ flattenList (List xs)

-- Problem 8: Eliminate consecutive duplicates of list elements.
-- Time complexity: 
-- Space complexity:
eliminateConsecutives :: (Eq a) => [a] -> [a]
eliminateConsecutives [] = []
eliminateConsecutives [x] = [x]
eliminateConsecutives (x:xs) = if x == head subList then subList else x : subList
    where subList = eliminateConsecutives xs

-- Time complexity: 
-- Space complexity:
eliminateConsecutives2 :: (Eq a) => [a] -> [a]
eliminateConsecutives2 (x:xs@(y:_))  
    | x == y = eliminateConsecutives2 xs
    | otherwise = x : eliminateConsecutives2 xs
eliminateConsecutives2 xs = xs

-- Problem 9: Pack consecutives. 
-- Time complexity: 
-- Space complexity:
packConsecutives :: (Eq a) => [a] -> [[a]]
packConsecutives [] = []
packConsecutives [x] = [[x]] 
packConsecutives (x:xs) 
    | x == z = (x:zall):ys
    | otherwise = [x]:yall
    where yall@(zall@(z:_):ys) = packConsecutives xs

-- Time complexity: 
-- Space complexity:
packConsecutives2 :: (Eq a) => [a] -> [[a]]
packConsecutives2 [] = []
packConsecutives2 xall@(x:xs) = let (first, rest) = span (==x) xall in first : packConsecutives2 rest 

-- Problem 10: Run-length encoding of a list.
-- Time complexity: 
-- Space complexity:
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\(x:xs) -> (length xs + 1, x)) . packConsecutives2