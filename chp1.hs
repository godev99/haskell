qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                  smaller = [a | a <- xs, a < x]
                  larger  = [b | b <- xs, b > x]


seqn :: Monad m => [m a] -> m [a]
seqn []         = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

-- 1.1
-- double double (2)
-- (double 2) + (double 2)


-- 1.2
-- sum[x] = x
-- x + sum[]
-- x + 0 
-- x


-- 1.3
product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * Main.product xs


-- 1.4
rqsort :: Ord a => [a] -> [a]
rqsort []     = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
                   where
                      smaller = [a | a <- xs, a <= x]
                      larger  = [b | b <- xs, b > x]

-- 1.5
-- any value equal to x would be discarded