-- 7.1
-- [f x | x <- xs, p x]
-- with map and filter

-- mapandfilter :: [a] -> a
-- mapandfilter = filter . map
-- mapandfilter p f xs = [f x | x <- xs, p x]
-- mapandfilter f g = \x -> f (g x)
-- mapandfilter xs = filter . (map xs) 
mp :: (a1 -> Bool) -> (a -> a1) -> [a] -> [a1]
mp p xs = filter p . map xs


--all' :: (a -> Bool) -> [Bool] -> Bool
--all' f [] = True
--all' f (x:xs) = if f x then all' f xs else False
all' p = and . map p
--all' f xs = filter f . [True | x <- xs, f x]  

--any' :: (a -> Bool) -> [Bool] -> Bool
any' p = or . map p

takewhile :: (a -> Bool) -> [a] -> [a]
takewhile _ [] = []
takewhile p (x:xs) | p x = x : takewhile p xs
                   | otherwise = []

dropwhile :: (a -> Bool) -> [a] -> [a]
dropwhile _ [] = []
dropwhile p (x:xs) | p x = dropwhile p xs
                   | otherwise = (x:xs)
--product' :: Num a => [a] -> a
product' x = foldl (\ x y -> x * y ) 1
--map' p (x:xs) = foldl (\x -> p x) []
--filter p 
