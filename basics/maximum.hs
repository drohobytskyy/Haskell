maximumL:: Ord a => [a] -> a
maximumL [x] = x
maximumL l@(h:t) | h >= head t = h
				 | otherwise = maximumL t
