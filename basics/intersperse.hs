interspersel :: a -> [a] -> [a]
interspersel a [] = []
interspersel x (h:t) | length (t) > 0 = h : x : interspersel x t
					 | otherwise = h : interspersel x t
