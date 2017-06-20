-- (*) :: Int -> Int -> Int
-- m * 0 = 0 
-- m * n = m + (m *(n-1))

-- drop :: Int -> [a] -> [a]
-- drop 0 xs = xs
-- drop _ [] = []
-- drop n (_:xs) = drop (n-1) xs


-- 6.1
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)
      | n < 0 = 0

-- 6.2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown x = x + sumdown (x-1)

-- 6.3
(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m Main.^ (n-1))

-- 6.4
euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | a < 0 = a
           | b < 0 = b
           | a > b = euclid (a-b) b
           | a < b = euclid a (b-a)

-- 6.5
{-
length [1,2,3]
1 + length [2,3]
1 + 1 + length [3]
1 + 1 + 1 + length []
1 + 1 + 1 + 0
= 3

drop 3 [1,2,3,4,5]
drop 2 [2,3,4,5]
drop 1 [3,4,5]
drop 0 [4,5]
= [4,5]

init [1,2,3]
1 : init [2,3]
1 : 2 : init [3]
1 : 2 : []
= [1,2]

-}

-- 6.6
and :: [Bool] -> Bool
and (x:xs) | null xs = if (x == True) then True else False
           | x == True = Main.and xs
           | otherwise = False

--concat :: [[a]] -> [a]
-- concat [[x],[xs]] = [x] : Main.concat [xs]   
-- concat xss = [x | xs <- xss, x <- xs]
--concat (x:xss) = x : Main.concat xss

replicate :: Int -> a -> [a]
replicate 0 x = []
replicate n x = x : Main.replicate (n-1) x

(!!!) :: [a] -> Int -> a
--(!!!) (x:_) 0 = x 
-- (!!!) (x:y) a = (!!!) y (a-1)
(!!!) (x:y) a | a <= 0 = x
              | otherwise = (!!!) y (a-1)
