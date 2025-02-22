module Code where

import Test.Hspec (describe, hspec, it, shouldBe)

-- Use the following data types for the questions below
data Tree a = Nil | TreeNode (Tree a) a (Tree a) deriving (Show, Eq)

data LinkedList a = Null | ListNode a (LinkedList a) deriving (Show, Eq)

data Direction = North | South | East | West deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- -----------------------------Category: Easy--------------------------------
-- ---------------------------------------------------------------------------

-- testing commit

matrixMultiplication :: [[Int]] -> [[Int]] -> [[Int]]
matrixMultiplication a b
  | null a || null b || null (head a) || null (head b) = [] 
  | length (head a) /= length b = []  
  | otherwise = multiply a b
  where
    transpose :: [[Int]] -> [[Int]]
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)

    multiply :: [[Int]] -> [[Int]] -> [[Int]]
    multiply m1 m2 =
      let transpose_m2 = transpose m2  
      in [[dotProduct row col | col <- transpose_m2] | row <- m1]

    dotProduct :: [Int] -> [Int] -> Int
    dotProduct v1 v2 = sum (zipWith (*) v1 v2)

    
-- Question 2


convertToList :: LinkedList a -> [a]
convertToList Null = []
convertToList (ListNode x xs) = x : convertToList xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

listStats :: LinkedList Int -> LinkedList Int
listStats Null = ListNode 0 (ListNode 0 Null) 
listStats linkedList = ListNode mean (ListNode median Null)
  where
    values = convertToList linkedList
    len = length values
    mean = sum values `div` len
    median = findMedian values

    findMedian xs
      | odd len   = sortedXs !! (len `div` 2)
      | otherwise = (sortedXs !! (len `div` 2 - 1) + sortedXs !! (len `div` 2)) `div` 2
      where
        sortedXs = quicksort xs

-- Question 3 

largestAdjacentSum :: [Int] -> (Int, Int, Int)
largestAdjacentSum xs = go xs (head xs, xs !! 1, xs !! 2) (sum $ take 3 xs)
  where
    go (a:b:c:remaining) largestTuple largestSum
      | currentSum > largestSum = go (b:c:remaining) (a, b, c) currentSum
      | otherwise           = go (b:c:remaining) largestTuple largestSum
      where
        currentSum = a + b + c
    go _ largestTuple _ = largestTuple 

-- Question 4

collatzConjecture :: Int -> (Int, Int)
collatzConjecture n = go n 0 n
  where
    go 1 steps maximum = (steps, maximum)  
    go x steps maximum
      | even x    = go (x `div` 2) (steps + 1) (max maximum x)
      | otherwise = go (3 * x + 1) (steps + 1) (max maximum x)


--Question 5

productExceptSelf :: [Int] -> [Int]
productExceptSelf nums = map (\i -> product (take i nums ++ drop (i+1) nums)) [0..length nums - 1]

-- Question 6

isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | otherwise = all (\x -> n `mod` x /= 0) [2 .. floor (sqrt (fromIntegral n))]

handleNonPrime :: [Int] -> [[Int]]
handleNonPrime [] = []
handleNonPrime xs = case span isPrime xs of
  ([], remaining)  -> handleNonPrime (drop 1 remaining)  
  (primes, remaining) -> primes : handleNonPrime remaining

longestPrimeSeq :: [Int] -> Int
longestPrimeSeq nums = maximum (0 : map length (filter (not . null) (handleNonPrime nums)))



-- ---------------------------------------------------------------------------
-- -----------------------------Category: Medium------------------------------
-- ---------------------------------------------------------------------------

-- Question 7

pruneLeaf :: Tree String -> String -> Tree String
pruneLeaf Nil _ = Nil
pruneLeaf (TreeNode Nil val Nil) parVal
  | substring val parVal = TreeNode Nil val Nil 
  | otherwise = Nil 
pruneLeaf (TreeNode left val right) parVal =
  TreeNode (pruneLeaf left val) val (pruneLeaf right val)

beginVal :: String -> String -> Bool
beginVal [] _ = True  
beginVal _ [] = False 
beginVal (x:xs) (y:ys) = x == y && beginVal xs ys  

substring :: String -> String -> Bool
substring [] _ = True 
substring _ [] = False 
substring sub str
  | beginVal sub str = True 
  | otherwise = substring sub (tail str)  

leafDeletion :: Tree String -> Tree String
leafDeletion Nil = Nil
leafDeletion (TreeNode Nil _ Nil) = Nil 
leafDeletion (TreeNode left val right) =
  let newLeft = pruneLeaf left val
      newRight = pruneLeaf right val
  in TreeNode newLeft val newRight



-- Question 8

processPath :: String -> String -> String -> String
processPath myString removed [] = reverse myString 
processPath [] _ ('#':xs) = processPath [] [] xs  
processPath (r:rs) removed ('#':xs) = processPath rs (r:removed) xs 
processPath myString [] ('@':xs) = processPath myString [] xs 
processPath myString (d:ds) ('@':xs) = processPath (d:myString) ds xs 
processPath myString removed (x:xs) = processPath (x:myString) removed xs 

textEditor :: String -> String
textEditor = processPath [] [] 


-- Question 9

silplify :: [String] -> [String] -> [String]
silplify acc [] = reverse acc
silplify [] ("..":xs) = silplify [] xs 
silplify acc ("..":xs) = silplify (drop 1 acc) xs
silplify acc ["."] = acc  
silplify acc (".":xs) = silplify acc xs  
silplify acc (x:xs) = silplify (x:acc) xs 

split :: String -> Char -> [String]
split [] _ = []
split s seperator = case break (== seperator) s of
    (chunk, []) -> [chunk]
    (chunk, _:remaining) -> chunk : split remaining seperator

join :: String -> [String] -> String
join _ [] = ""
join _ [x] = x
join seperator (x:xs) = x ++ seperator ++ join seperator xs

halkiOs :: String -> String
halkiOs path = "/" ++ join "/" (silplify [] (filter (not . null) (split path '/')))


-- Question 10


leastSwaps :: String -> Int
leastSwaps s
    | not (isPalindrome s) = -1  
    | otherwise = countSwaps (convertToList s) 0
  where
    convertToList = id  

isPalindrome :: String -> Bool
isPalindrome s = length (filter odd (map count (unique s))) <= 1
  where
    unique [] = []
    unique (x:xs) = x : unique (filter (/= x) xs)
    count c = length (filter (== c) s)

countSwaps :: [Char] -> Int -> Int
countSwaps [] swaps = swaps
countSwaps [x] swaps = swaps  
countSwaps xs swaps =
    let v = last xs
        pos = getPos v (init xs)  
    in if pos == -1
       then countSwaps (removeOdd xs) swaps  
       else countSwaps (removeProcessed pos xs) (swaps + pos)

getPos :: Char -> [Char] -> Int
getPos _ [] = -1
getPos c (x:xs) = if c == x then 0 else 1 + getPos c xs

removeProcessed :: Int -> [Char] -> [Char]
removeProcessed i xs = take i xs ++ drop (i + 1) (init xs)  

removeOdd :: [Char] -> [Char]
removeOdd xs = take mid xs ++ drop (mid + 1) xs
  where mid = length xs `div` 2

palindromeSwaps :: [String] -> Int
palindromeSwaps strs = sum (map leastSwaps strs)



-- Question 11


findMaxStreak :: [Int] -> Int -> Int -> Int
findMaxStreak [] maxStreak currentStreak = max maxStreak currentStreak
findMaxStreak [_] maxStreak currentStreak = max maxStreak currentStreak
findMaxStreak (x:y:remaining) maxStreak currentStreak
  | y == x = findMaxStreak (y:remaining) maxStreak currentStreak  
  | y == x + 1 = findMaxStreak (y:remaining) maxStreak (currentStreak + 1) 
  | otherwise = findMaxStreak (y:remaining) (max maxStreak currentStreak) 1 

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (p:xs) = quickSort [x | x <- xs, x < p] ++ [p] ++ quickSort [x | x <- xs, x >= p]


maxStreak :: [Int] -> Int
maxStreak [] = 0
maxStreak xs = findMaxStreak (quickSort xs) 1 1

-- ---------------------------------------------------------------------------
-- -----------------------------Category: Hard--------------------------------
-- ---------------------------------------------------------------------------

-- Question 12


sumOfTree :: Num a => Tree a -> a
sumOfTree Nil = 0
sumOfTree (TreeNode left val right) = sumOfTree left + val + sumOfTree right

changeTree :: Num a => Tree a -> Tree a
changeTree Nil = Nil
changeTree (TreeNode left val right) =
  TreeNode (changeTree left) (sumOfTree right - sumOfTree left) (changeTree right)

handleZeroLeaf :: (Eq a, Num a) => Tree a -> Tree a
handleZeroLeaf Nil = Nil
handleZeroLeaf (TreeNode left val right)
  | isLeaf prunedLeft prunedRight && val == 0 = Nil  
  | otherwise = TreeNode prunedLeft val prunedRight
  where
    prunedLeft = handleZeroLeaf left
    prunedRight = handleZeroLeaf right
    isLeaf Nil Nil = True
    isLeaf _ _ = False

pruneTreeUntilStable :: (Eq a, Num a) => Tree a -> Tree a
pruneTreeUntilStable tree =
  let pruned = handleZeroLeaf tree
  in if pruned == tree then tree else pruneTreeUntilStable pruned


treeDeduction :: (Eq a, Num a) => Tree a -> Tree a
treeDeduction tree =
  let transformedTree = changeTree tree
  in pruneTreeUntilStable transformedTree


-- Question 13


caculateDist :: (Int, Int) -> (Int, Int) -> Int
caculateDist (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

makeGraph :: [(Int, Int, Int)] -> [(Int, [Int])]
makeGraph containers =
  [(i, [j | (j, (x2, y2, _)) <- indexed, i /= j, caculateDist (x1, y1) (x2, y2) <= r^2])
  | (i, (x1, y1, r)) <- indexed]
  where
    indexed = zip [0..] containers

dfs :: [(Int, [Int])] -> Int -> [Int] -> Int
dfs graph node visited
  | node `elem` visited = 0
  | otherwise =
      let newVisited = node : visited
          neighbors = lookupOrEmpty node graph
      in 1 + sum (map (\n -> dfs graph n newVisited) neighbors)

lookupOrEmpty :: Int -> [(Int, [Int])] -> [Int]
lookupOrEmpty key graph = case lookup key graph of
  Just v  -> v
  Nothing -> []

poisonSpill :: [(Int, Int, Int)] -> Int
poisonSpill containers
  | null containers = 0  
  | otherwise =
      let graph = makeGraph containers
      in maximum [dfs graph i [] | i <- [0..length containers - 1]]


-- -- Question 14

type Position = (Int, Int)
findPositions :: [[Char]] -> (Position, Position)
findPositions maze = ((sx, sy), (gx, gy))
  where
    positions = [((r, c), cell) | (r, row) <- zip [0..] maze, (c, cell) <- zip [0..] row]
    (sx, sy) = fst . head $ filter (\(_, cell) -> cell == 'S') positions
    (gx, gy) = fst . head $ filter (\(_, cell) -> cell == 'G') positions

bfs :: [[Char]] -> Position -> Position -> [Direction]
bfs maze start goal = search [(start, [])] []
  where
    rows = length maze
    cols = length (head maze)
    
    directions = [(North, (-1, 0)), (South, (1, 0)), (West, (0, -1)), (East, (0, 1))]
    isValid (r, c) = r >= 0 && c >= 0 && r < rows && c < cols && (maze !! r !! c /= 'X')
    search [] _ = [] 
    search ((pos, path):queue) visited
      | pos == goal = path 
      | pos `elem` visited = search queue visited 
      | otherwise =
          let newVisited = pos : visited
              newQueue = queue ++ [(nextPos, path ++ [dir]) | (dir, (dr, dc)) <- directions,
                          let nextPos = (fst pos + dr, snd pos + dc),
                          isValid nextPos, nextPos `notElem` newVisited]
          in search newQueue newVisited

mazePathFinder :: [[Char]] -> [Direction]
mazePathFinder maze =
  let (start, goal) = findPositions maze
  in bfs maze start goal


-- Question 15


halloweenEscape :: [(String, String)] -> Int
halloweenEscape = undefined

-- Main Function
main :: IO ()
main =
  hspec $ do
    -- Test Matrix Multiplication
    describe "matrixMultiplication" $ do
      it "should return product of two matrices (lists)" $ do
        matrixMultiplication [[1, 2], [3, 4]] [[2, 0], [1, 2]] `shouldBe` [[4, 4], [10, 8]]
        matrixMultiplication [[1, 0], [0, 3]] [[4], [2]] `shouldBe` [[4], [6]]
        matrixMultiplication [[1, 2, 6], [0, 3, 4]] [[2, 0], [2, 7], [-5, 6]] `shouldBe` [[-24, 50], [-14, 45]]
        matrixMultiplication [[1, 2], [3, 4]] [[2, 0]] `shouldBe` []
        matrixMultiplication [[1, 7], [3, 1], [1, 5]] [[5, 6], [7, 8], [2, 5]] `shouldBe` []
        matrixMultiplication [[1, 7], [4, 3], [2, 5], [-3, 0]] [[5, -6, 6], [-7, 14, 8]] `shouldBe` [[-44, 92, 62], [-1, 18, 48], [-25, 58, 52], [-15, 18, -18]]
        matrixMultiplication [[1], [2], [3]] [[4]] `shouldBe` [[4], [8], [12]]
        matrixMultiplication [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]] [[1, 2, 3, 4, 5, 6, 7, 8], [9, 10, 11, 12, 13, 14, 15, 16]] `shouldBe` [[19, 22, 25, 28, 31, 34, 37, 40], [39, 46, 53, 60, 67, 74, 81, 88], [59, 70, 81, 92, 103, 114, 125, 136], [79, 94, 109, 124, 139, 154, 169, 184], [99, 118, 137, 156, 175, 194, 213, 232]]

    -- Test Stats of a LinkedList
    describe "listStats" $ do
      it "should return the mean and median of the linkedlist in linkedlist form" $ do
        listStats (ListNode 3 (ListNode 5 (ListNode 1 (ListNode 9 Null)))) `shouldBe` ListNode 4 (ListNode 4 Null)
        listStats (ListNode 5 (ListNode 1 (ListNode 9 Null))) `shouldBe` ListNode 5 (ListNode 5 Null)
        listStats (ListNode 3 Null) `shouldBe` ListNode 3 (ListNode 3 Null)

    -- Test Largest Adjacent Sum
    describe "largestAdjacentSum" $ do
      it "should return the three adjacent numbers with the largest sum" $ do
        largestAdjacentSum [2, 4, 1, 6, 4, 3, 7, 2] `shouldBe` (4, 3, 7)
        largestAdjacentSum [1, 2, 3, 4, 5, 6] `shouldBe` (4, 5, 6)
        largestAdjacentSum [1, -1, 2, 3, 5, -3] `shouldBe` (2, 3, 5)
        largestAdjacentSum [10, 1, 2, 3, 5, -3] `shouldBe` (10, 1, 2)
        largestAdjacentSum [-1, -2, -3, -4, -5, -6] `shouldBe` (-1, -2, -3)
        largestAdjacentSum [5, 1, 2, 10, 3, 4] `shouldBe` (10, 3, 4)
        largestAdjacentSum [5, 5, 5, 4, 5, 6] `shouldBe` (5, 5, 5)
        largestAdjacentSum [5, 5, 5, 4, 5, 6, 7, 7, 6] `shouldBe` (6, 7, 7)
        largestAdjacentSum [3, 3, 3, 3, 3, 3] `shouldBe` (3, 3, 3)

    -- Test Collatz Conjecture
    describe "collatzConjecture" $ do
      it "should return a tuple of step count and max value" $ do
        collatzConjecture 6 `shouldBe` (8, 16)
        collatzConjecture 12 `shouldBe` (9, 16)
        collatzConjecture 4 `shouldBe` (2, 4)
        collatzConjecture 5 `shouldBe` (5, 16)
        collatzConjecture 11 `shouldBe` (14, 52)
        collatzConjecture 17 `shouldBe` (12, 52)
        collatzConjecture 1 `shouldBe` (0, 1)

    -- Test Array Product Except Self
    describe "productExceptSelf" $ do
      it "should return the products list" $ do
        productExceptSelf [2, 3, 4, 5] `shouldBe` [60, 40, 30, 24]
        productExceptSelf [-1, 2, -3, 4] `shouldBe` [-24, 12, -8, 6]
        productExceptSelf [1, 2, 0, 4, 5] `shouldBe` [0, 0, 40, 0, 0]
        productExceptSelf [7, 3, 10, 2] `shouldBe` [60, 140, 42, 210]
        productExceptSelf [5, -2, 4, -3, 6] `shouldBe` [144, -360, 180, -240, 120]

    -- Test Longest Consecutive Sequence
    describe "longestPrimeSeq" $ do
      it "should return the count of longest consecutive seq of prime numbers" $ do
        longestPrimeSeq [2, 3, 5, 7, 6, 11, 4, 17, 6] `shouldBe` 4
        longestPrimeSeq [10, 15, 3, 7, 11, 6, 9, 11, 13] `shouldBe` 3
        longestPrimeSeq [4, 6, 8, 10] `shouldBe` 0
        longestPrimeSeq [2, 3, 5, 7, 11, 13, 17] `shouldBe` 7
        longestPrimeSeq [10, 2, 3, 4, 5, 6, 7, 8, 11] `shouldBe` 2
        longestPrimeSeq [8, 10, 15, 6, 11, 13, 17, 19, 23] `shouldBe` 5

    -- Test Leaf Deletion
    describe "leafDeletion" $ do
      it "should return the modified tree with the deleted non-substring leaves" $ do
        leafDeletion (TreeNode (TreeNode (TreeNode Nil "act" Nil) "cat" (TreeNode Nil "ca" Nil)) "root" (TreeNode Nil "to" Nil)) `shouldBe` TreeNode (TreeNode Nil "cat" (TreeNode Nil "ca" Nil)) "root" Nil
        leafDeletion (TreeNode (TreeNode Nil "cats" Nil) "root" Nil) `shouldBe` TreeNode Nil "root" Nil
        leafDeletion (TreeNode (TreeNode (TreeNode Nil "at" Nil) "cats" (TreeNode Nil "catts" Nil)) "root" (TreeNode Nil "oo" Nil)) `shouldBe` TreeNode (TreeNode (TreeNode Nil "at" Nil) "cats" Nil) "root" (TreeNode Nil "oo" Nil)
        leafDeletion (TreeNode (TreeNode (TreeNode Nil "dog" Nil) "cats" (TreeNode Nil "elephant" Nil)) "root" (TreeNode Nil "apple" Nil)) `shouldBe` TreeNode (TreeNode Nil "cats" Nil) "root" Nil
        leafDeletion (TreeNode (TreeNode (TreeNode Nil "dog" Nil) "Simba" (TreeNode Nil "Sim" Nil)) "root" (TreeNode Nil "apple" Nil)) `shouldBe` TreeNode (TreeNode Nil "Simba" (TreeNode Nil "Sim" Nil)) "root" Nil
        leafDeletion (TreeNode (TreeNode (TreeNode Nil "abc" Nil) "abcdef" (TreeNode Nil "de" Nil)) "root" (TreeNode (TreeNode Nil "oof" Nil) "floof" (TreeNode Nil "floo" Nil))) `shouldBe` TreeNode (TreeNode (TreeNode Nil "abc" Nil) "abcdef" (TreeNode Nil "de" Nil)) "root" (TreeNode (TreeNode Nil "oof" Nil) "floof" (TreeNode Nil "floo" Nil))
        leafDeletion (TreeNode (TreeNode (TreeNode Nil "ui" Nil) "fruits" (TreeNode Nil "banana" Nil)) "fruit basket" (TreeNode (TreeNode Nil "orange" Nil) "Tangerine" (TreeNode Nil "anger" Nil))) `shouldBe` TreeNode (TreeNode (TreeNode Nil "ui" Nil) "fruits" Nil) "fruit basket" (TreeNode Nil "Tangerine" (TreeNode Nil "anger" Nil))
        leafDeletion (TreeNode Nil "root" (TreeNode Nil "vegetable" (TreeNode Nil "potato" (TreeNode Nil "tomato" (TreeNode Nil "shakarkandi" (TreeNode Nil "shakar" Nil)))))) `shouldBe` TreeNode Nil "root" (TreeNode Nil "vegetable" (TreeNode Nil "potato" (TreeNode Nil "tomato" (TreeNode Nil "shakarkandi" (TreeNode Nil "shakar" Nil)))))

    -- Test Text Editor
    describe "textEditor" $ do
      it "should return the updated string" $ do
        textEditor "text#edi@tor" `shouldBe` "texedittor"
        textEditor "hello###wor@@ld" `shouldBe` "heworllld"
        textEditor "abcde#fg@h" `shouldBe` "abcdfgeh"

    -- Test Halki si OS
    describe "halkiOs" $ do
      it "should return the simplified path" $ do
        halkiOs "/users/" `shouldBe` "/users"
        halkiOs "/work//files/" `shouldBe` "/work/files"
        halkiOs "/documents///code/" `shouldBe` "/documents/code"
        halkiOs "/projects/reports/../images" `shouldBe` "/projects/images"
        halkiOs "/archive/files/./photos" `shouldBe` "/archive/files/photos"
        halkiOs "/home//user///downloads" `shouldBe` "/home/user/downloads"
        halkiOs "/root" `shouldBe` "/root"
        halkiOs "/" `shouldBe` "/"
        halkiOs "/videos/movies/../music" `shouldBe` "/videos/music"
        halkiOs "/abc/def/ghi" `shouldBe` "/abc/def/ghi"
        halkiOs "/abc/./def/.." `shouldBe` "/abc"
        halkiOs "/one/two/three/four/" `shouldBe` "/one/two/three/four"
        halkiOs "/one//two/.//three/" `shouldBe` "/one/two/three"
        halkiOs "/foo_bar/.." `shouldBe` "/"
        halkiOs "/hello123/./world456" `shouldBe` "/hello123/world456"

    -- Test Palindromic Paths with Minimum Swaps
    describe "palindromeSwaps" $ do
      it "should return the sum of swap counts to make each palindrome" $ do
        palindromeSwaps ["arcerac", "banana", "bbo"] `shouldBe` 2
        palindromeSwaps ["banana", "apple", "orange", "relrplepsee"] `shouldBe` 10
        palindromeSwaps ["aabbccd"] `shouldBe` 9

    -- Test Maximum Streak of Consecutive Numbers
    describe "maxStreak" $ do
      it "should return the length of the longest sequence of consecutive sequence" $ do
        maxStreak [100, 4, 200, 1, 3, 2] `shouldBe` 4
        maxStreak [9, 1, 3, 10, 2, 20] `shouldBe` 3
        maxStreak [10, 20, 30, 40] `shouldBe` 1
        maxStreak [5, 2, 99, 3, 1, 4, 100] `shouldBe` 5
        maxStreak [8, 7, 6, 5, 4, 3, 2, 1] `shouldBe` 8

    -- Test Tree Deduction
    describe "treeDeduction" $ do
      it "should return the tree after deductions" $ do
        treeDeduction (TreeNode (TreeNode (TreeNode Nil 4 Nil) 2 (TreeNode Nil 5 Nil)) 1 (TreeNode (TreeNode Nil 6 Nil) 3 (TreeNode Nil 7 Nil))) `shouldBe` (TreeNode (TreeNode Nil 1 Nil) 5 (TreeNode Nil 1 Nil))
        treeDeduction (TreeNode (TreeNode (TreeNode Nil 10 Nil) 5 Nil) 2 Nil) `shouldBe` (TreeNode (TreeNode Nil (-10) Nil) (-15) Nil)
        treeDeduction (TreeNode (TreeNode Nil 0 Nil) 0 Nil) `shouldBe` Nil
        treeDeduction (TreeNode (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 5 Nil)) 1 (TreeNode Nil 3 Nil)) `shouldBe` TreeNode Nil (-9) Nil

    -- Test Citywide Poison Spill
    describe "poisonSpill" $ do
      it "should return the max containers that can be affected" $ do
        poisonSpill [(0, 0, 3), (2, 0, 2), (4, 0, 2), (6, 0, 2), (8, 0, 1)] `shouldBe` 5
        poisonSpill [(2, 1, 3), (6, 1, 4)] `shouldBe` 2
        poisonSpill [(1, 1, 5), (10, 10, 5)] `shouldBe` 1

    -- Test Bilal Trapped in a Maze
    describe "mazePathFinder" $ do
      it "should return the shortest escape path" $ do
        mazePathFinder ["SOOOO", "OXXXO", "OOOXO", "XXOXO", "OGOOO"] `shouldBe` [South, South, East, East, South, South, West]
        mazePathFinder ["SOOO", "OOXO", "OXOX", "OXOX", "OOOG"] `shouldBe` [South, South, South, South, East, East, East]

    -- Test Halloween Escape
    describe "halloweenEscape" $ do
      it "should return the minimum number of moves to reach the end" $ do
        halloweenEscape [("House 1", "Closet"), ("House 1", "Basement"), ("House 2", "Closet"), ("House 3", "Attic"), ("House 3", "Bathroom"), ("House 2", "Basement"), ("House 2", "Attic")] `shouldBe` 3
        halloweenEscape [("House 2", "Dining Room"), ("House 1", "Attic"), ("House 1", "Kitchen"), ("House 1", "Dining Room")] `shouldBe` 1
