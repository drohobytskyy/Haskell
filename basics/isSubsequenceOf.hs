isSubsequenceOfL :: Eq a => [a] -> [a] -> Bool
isSubsequenceOfL [] _ = True
isSubsequenceOfL _ [] = False
isSubsequenceOfL x@(h1:t1) y@(h2:t2) | h1 == h2 = isSubsequenceOfL t1 t2
									 | otherwise = isSubsequenceOfL x t2
