-- Problem 21: Insert an element at a given position into a list.
-- Time complexity: 
-- Space complexity:
insertAt :: Int -> a -> [a] -> [a]
insertAt n _ lst | n < 0 || n >= length lst = lst
insertAt 1 toInsert lst = toInsert : lst
insertAt n toInsert (x:xs) = x : insertAt (n-1) toInsert xs

-- Time complexity: 
-- Space complexity:
insertAt2 :: Int -> a -> [a] -> [a]
insertAt2 n toInsert = foldr helper [] . zip [1..]
    where 
        helper (i, x) acc =  if i == n then toInsert:x:acc else x:acc

-- Problem 22: Create a list containing all integers within a given range.
-- Time complexity: 
-- Space complexity:
myRange :: Int -> Int -> [Int]
myRange a b = take (b - a + 1) $ iterate (+1) a

-- Time complexity: 
-- Space complexity:
myRange2 :: Int -> Int -> [Int]
myRange2 a b = take (b - a + 1) $ scanl (+) a $ repeat 1

-- Problem 23: Extract a given number of randomly selected elements from a list.
-- Time complexity: 
-- Space complexity:
randomSelect :: [a] -> Int -> []

-------------------------
-- Problem n:
-- Time complexity: 
-- Space complexity:

main :: IO()
main = do 
    let x = 1 
    print $ myRange2 5 10