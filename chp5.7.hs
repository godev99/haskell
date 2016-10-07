concat :: [[a]] -> [a]
concat xxs = [x | xs <- xxs, x <- xs]

-- test :: Int -> [(Int,Int)]
-- test x = [(y,x) | y <- [1..x], x /= y]
        
test :: Int -> [(Int,Int)]
test x = [(y,z) | y <- [1..t-1], z <- [t..x]]
   where t = (div x 2) + 1

-- test :: Int -> [(Int,Int)]
-- test = Main.concat [[(x,y) | x <- [1,2] | y <- [3,4]]
