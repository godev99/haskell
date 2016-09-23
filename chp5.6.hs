divisor :: Int -> [Int]
divisor n = [x | x <- [1..n-1], n `mod` x == 0]

perfect :: Int -> [Int]
perfect n = [x | x <- [1..n], sum (divisor x) == x]


