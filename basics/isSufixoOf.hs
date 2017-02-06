isSufixoOf :: Eq a => [a] -> [a] -> Bool
isSufixoOf [] _ = True
isSufixoOf l1 l2 | last l1 == last l2 = isSufixoOf (init l1) (init l2)
				 | otherwise = False
