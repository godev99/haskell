find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

-- positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i | (x',i) <- zip xs [0..], x == x']

-- positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i | (x',i) <- zip xs [0..]]

-- positions :: Int -> [Int] -> [(Int,Int)]
-- positions a b = zip b [c | c <- b, a == c]

-- positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i | (x',i) <- zip xs [0..]]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [1..n])
          where n = length xs
