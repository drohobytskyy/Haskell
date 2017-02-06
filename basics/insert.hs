insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x l@(h:t) | x < h = x:l
				 | otherwise = h : insert x t
