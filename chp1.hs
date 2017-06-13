qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                  smaller = [a | a <- xs, a <= x]
                  larger  = [b | b <- xs, b > x]

seqn []         = return []
seqn (act:acts) = do x <- act
                       xs <- seqn acts
                       return (x:xs)
