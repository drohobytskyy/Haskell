unionL :: Eq a => [a] -> [a] -> [a]
unionL [] x = x
unionL l1@(h1:t1) l2@(h2:t2) | h1 == h2 = h1 : unionL t1 t2
							 | otherwise = h1 : unionL t1 l2
