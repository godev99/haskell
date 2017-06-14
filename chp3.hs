{-
-- 3.1

['a','b','c'] :: [Int]
('a','b','c') :: (Int)
[(False,'0'),(True,'1')] :: [(Bool,Char)]
([False,True],['0','1']) :: ([Bool],[Char])
[tail, init, reverse] :: [[a] -> [a]]

-}

-- 3.2
bools :: [Bool]
bools = [False,True]

nums :: [[Int]]
nums = [[1],[2],[3],[4],[5]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy a = (a,a)

apply :: (a -> b) -> a -> b
apply f x = f x

-- 3.3
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => Int -> Int
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t 
twice f x = f (f x)
