replicate :: Int -> a -> [a]
replicate n a = [a | x <- [1..n]]
