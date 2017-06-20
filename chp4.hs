-- 4.1
halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs
{-
halve xs = splitAt len xs
            where
                len = length xs `div` 2
-}

-- 4.2
third :: [a] -> a
-- third xs = head (tail(tail xs))
-- third xs = xs !! 2
third (_:_:x:_) = x

-- 4.3
safetail :: [a] -> [a]
-- safetail xs = if (null xs) then xs else tail xs
-- safetail xs | null xs   = xs
--             | otherwise = tail xs
safetail [] = []
safetail xs = tail xs

-- 4.4
(||) :: Bool -> Bool -> Bool

--(||) True _ = True
--(||) _ True = True

(||) False b = b
(||) b False = b

-- 4.5
-- (&&) :: Bool -> Bool -> Bool
-- (&&) a b = if (a == True Prelude.&& b == True) then True else False

-- 4.6
(&&) :: Bool -> Bool -> Bool
(&&) a b = if (a == True) then b else False

-- 4.7
mult :: Int -> Int -> Int -> Int
--mult x y z = x*y*z
mult = \x -> (\y -> (\z -> x*y*z))

luhnDouble :: Int -> Int
luhnDouble x | x < 5 = x+x
			 | otherwise = x+x-9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = if mod ((luhnDouble a) + b + (luhnDouble c) + d) 10 > 0 
                 then False
                 else True
