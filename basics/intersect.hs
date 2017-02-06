intersectL :: Eq a => [a] ->[a] -> [a]
intersectL [] x = []
intersectL x@(h1:t1) y@(h2:t2) | aux(h1) (y) = h1 : intersectL t1 y
							   | otherwise = intersectL t1 y
aux k [] = False
aux k (p1:p2) = if (k==p1) then True else aux k p2
