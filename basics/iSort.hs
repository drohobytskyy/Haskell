iSorti :: Ord a => [a] -> [a]
iSorti [] = []
iSorti l@(x:xs) = iSorti [y|y <- xs, y < x] ++ [x] ++ iSorti [y|y <- xs, y >= x]
