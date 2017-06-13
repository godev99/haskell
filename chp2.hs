-- 2.2
{- 
    2^3*4 -> (2^3)*4
    2*3+4*5 -> (2*3)+(4*5)
    2+3*4^5 -> 2+(3*(4^5))
-}

-- 2.3
numb = a `div` length xs
      where
        a = 10
        xs = [1,2,3,4,5]

-- 2.4
last :: Num t => [t] -> t
last [] = 0
-- last xs = xs !! (length xs - 1)
last xs = head (reverse xs) 

init [] = []
--init xs = take (length xs - 1) xs
init xs = reverse (tail (reverse xs)) 