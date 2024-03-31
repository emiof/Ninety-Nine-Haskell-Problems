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

-- Problem 24: Lotto: Draw N different random numbers from the set 1..M.
-- Time complexity: 
-- Space complexity:

-- Problem 25: Generate a random permutation of the elements of a list.
-- Time complexity: 
-- Space complexity:

-- Problem 26: Generate combinations of K distinct objects chosen from the N elements of a list.
-- Time complexity: 
-- Space complexity:
combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations k lst = concatMap (\(x:xs) -> [x:comb | comb <- combinations (k-1) xs]) $ take (length lst) $ iterate tail lst

-- Time complexity: 
-- Space complexity:
combinations2 :: (Eq a) => Int -> [a] -> [[a]]
combinations2 k lst = helper k lst []
    where
        helper :: (Eq a) => Int -> [a] -> [a] -> [[a]]
        helper 0 _ currComb = [currComb]
        helper _ [] _ = [] 
        helper k (x:xs) currComb = helper (k-1) xs (x:currComb) ++ helper k xs currComb

-- Time complexity: 
-- Space complexity:
combinations3 :: (Eq a) => Int -> [a] -> [[a]]
combinations3 0 _ = [[]]
combinations3 k lst = [lst !! i : x | i <- [0..(length lst - 1)], x <- combinations3 (k - 1) (drop (i + 1) lst)]

-- Problem 27: Group the elements of a set into disjoint subsets.
-- Time complexity: 
-- Space complexity:
applyAtIndex :: (Int -> Int) -> Int -> [Int] -> [Int]
applyAtIndex f i  = zipWith (\i_ e -> if i_ == i then f e else e) [0..] 

group :: [Int] -> [a] -> [[[a]]]
group gps elems | length elems < sum gps =  []
group gps [] = [replicate (length gps) []]
group gps (x:xs) = (concatMap (\(i, g) -> if g > 0 then addToGroup i x $ group (applyAtIndex (subtract 1) i gps) xs else []) $ zip [0..] gps) ++ group gps xs
    where
        addToGroup :: Int -> a -> [[[a]]] -> [[[a]]]
        addToGroup i e  =  map (\g -> zipWith (\i_ g_ -> if i_ == i then e:g_ else g_) [0..] g)

-------------------------
-- Problem n:
-- Time complexity: 
-- Space complexity:

main :: IO()
main = do 
    print $ group [1, 2] [1, 2, 3, 4]
