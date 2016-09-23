yop :: Int -> [(Int,Int)]
yop a = [(x,y) | n <- [1..a]]
         where if n <= ((length n) `mod` 2) then y == n else x == n
