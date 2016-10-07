import Data.Char

-- 5.1
square :: [Int]
square = [x*2 | x <- [0..100]]


-- 5.2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..n], y <- [0..m]]


-- 5.3
newsquare :: Int -> [(Int,Int)]
newsquare n = [(v,k) | (v,k) <- grid n n, v /= k]


-- 5.4
replicate :: Int -> a -> [a]
replicate n a = [a | x <- [1..n]]


-- 5.5
pyth :: Int -> [(Int,Int,Int)]
pyth n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], (x*x + y*y) == z*z]


-- 5.6
divisor :: Int -> [Int]
divisor n = [x | x <- [1..n-1], n `mod` x == 0]

perfect :: Int -> [Int]
perfect n = [x | x <- [1..n], sum (divisor x) == x]


-- 5.7
concat' = concat [[(x,y) | x <- [1,2,3]] | y <- [4,5,6]]


-- 5.8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [1..n])
          where n = length xs
          
-- 5.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct x y = sum (concat [x,y])


-- 5.10
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'A' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, toLower x == toLower x']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs

let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a'
          | isUpper c = ord c - ord 'A'

int2let :: Int ->Char
int2let n = chr (ord 'a' + n)

int2let_isupper :: Int ->Char
int2let_isupper n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = int2let_isupper ((let2int c + n) `mod` 26)
          | otherwise = c
          
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
 
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2) /e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs 
