tab :: Eq a => [a] -> [a] -> [a]
tab l [] = l
tab x@(h1:t1) y@(h2:t2) | h1 == h2 = tab t1 t2
						| otherwise = h1:tab t1 y
