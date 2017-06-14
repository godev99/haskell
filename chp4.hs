halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs
{-
halve xs = splitAt len xs
            where
                len = length xs `div` 2
-}

