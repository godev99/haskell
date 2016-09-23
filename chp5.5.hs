pyth :: Int -> [(Int,Int,Int)]
pyth n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], (x*x + y*y) == z*z]
