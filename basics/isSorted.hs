isSorted :: Ord a => [a] -> Bool
isSorted l@(h:t) | length (t) > 0 = if h <= head t then isSorted t else False
				 | otherwise = True
