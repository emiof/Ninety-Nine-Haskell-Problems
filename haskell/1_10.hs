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

-- Simple solution (reverseElements):
--   Time complexity: O(n^2) due to repeated (++) traversals.
--   Space complexity: O(n) for the recursion stack plus the result list.
-- Advanced solution (reverseElements_advanced):
--   Time complexity: O(n) using a tail-recursive accumulator.
--   Space complexity: O(n) for the resulting list with only O(1) extra stack usage.
reverseElements :: [a] -> [a]
reverseElements [] = []
reverseElements (x:xs) = reverseElements xs ++ [x]

reverseElements_advanced :: [a] -> [a]
reverseElements_advanced = go []
  where
    go acc [] = acc
    go acc (x:xs) = go (x : acc) xs

-- Time complexity:
-- Space complexity:
reverseElements2 :: [a] -> [a]
reverseElements2 = foldl (flip (:)) []

-- Problem 6: Find out whether a list is a palindrome.
-- Simple solution (isPalindrome):
--   Time complexity: O(n^2) from repeated uses of init and last.
--   Space complexity: O(n) for recursion, holding shrinking suffixes.
-- Advanced solution (isPalindrome_advanced):
--   Time complexity: O(n) by building one reversed copy and comparing once.
--   Space complexity: O(n) to store the reversed list with constant extra stack.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)

isPalindrome_advanced :: (Eq a) => [a] -> Bool
isPalindrome_advanced xs = xs == reverseElements_advanced xs

-- Problem 7:  Flatten a nested list structure.
-- Simple solution (flattenList):
--   Time complexity: O(n^2) in the worst case due to repeated (++) reconstruction.
--   Space complexity: O(n) in the call stack depth proportional to nesting.
-- Advanced solution (flattenList_advanced):
--   Time complexity: O(n) by threading a difference-list accumulator.
--   Space complexity: O(n) for the output while maintaining tail recursion in the accumulator.
data NestedList a = Elem a | List [NestedList a]
flattenList :: NestedList a -> [a]
flattenList (Elem a) = [a]
flattenList (List []) = []
flattenList (List (x:xs)) = flattenList x ++ flattenList (List xs)

flattenList_advanced :: NestedList a -> [a]
flattenList_advanced nested = go nested []
  where
    go (Elem a) acc = a : acc
    go (List xs) acc = foldr go acc xs

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
-- Simple solution (packConsecutives):
--   Time complexity: O(n^2) worst-case because of (++) as the recursion unwinds.
--   Space complexity: O(n) to hold intermediate suffixes during reconstruction.
-- Advanced solution (packConsecutives_advanced):
--   Time complexity: O(n) by processing once with a right fold.
--   Space complexity: O(n) for the result while using only O(1) additional stack.
packConsecutives :: (Eq a) => [a] -> [[a]]
packConsecutives [] = []
packConsecutives [x] = [[x]]
packConsecutives (x:xs)
    | x == z = (x:zall):ys
    | otherwise = [x]:yall
    where yall@(zall@(z:_):ys) = packConsecutives xs

packConsecutives_advanced :: (Eq a) => [a] -> [[a]]
packConsecutives_advanced = foldr step []
  where
    step x [] = [[x]]
    step x acc@(group:rest)
        | x == head group = (x : group) : rest
        | otherwise = [x] : acc

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

-- QuickCheck-style regression tests to ensure advanced implementations match simple ones.
test_reverseElements_advanced :: Bool
test_reverseElements_advanced = all check sampleLists
  where
    sampleLists = [ ([] :: [Int])
                  , [1]
                  , [1,2]
                  , [1,2,3,4]
                  , replicate 5 42
                  ]
    check xs = reverseElements_advanced xs == reverseElements xs

test_isPalindrome_advanced :: Bool
test_isPalindrome_advanced = all check sampleLists
  where
    sampleLists = [ ([] :: [Int])
                  , [1]
                  , [1,1]
                  , [1,2,1]
                  , [1,2,3]
                  , [1,2,2,1]
                  ]
    check xs = isPalindrome_advanced xs == isPalindrome xs

test_flattenList_advanced :: Bool
test_flattenList_advanced = all check sampleNested
  where
    sampleNested =
        [ List []
        , Elem (1 :: Int)
        , List [Elem 1, Elem 2, Elem 3]
        , List [Elem 1, List [Elem 2, List [Elem 3]], Elem 4]
        , List [List [], Elem 5]
        ]
    check nl = flattenList_advanced nl == flattenList nl

test_packConsecutives_advanced :: Bool
test_packConsecutives_advanced = all check sampleLists
  where
    sampleLists = [ ([] :: [Int])
                  , [1]
                  , [1,1,1]
                  , [1,2,3]
                  , [1,1,2,2,3,3,3,2,2]
                  ]
    check xs = packConsecutives_advanced xs == packConsecutives xs

advancedImplementationsMatchSimple :: Bool
advancedImplementationsMatchSimple =
    and [ test_reverseElements_advanced
        , test_isPalindrome_advanced
        , test_flattenList_advanced
        , test_packConsecutives_advanced
        ]
