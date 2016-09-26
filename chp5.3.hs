grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..n], y <- [0..m]]

square :: Int -> [(Int,Int)]
square n = [(v,k) | (v,k) <- grid n n, v /= k]

