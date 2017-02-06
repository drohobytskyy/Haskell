elemMSet :: Eq a => a ->[(a,Int)] -> Bool
elemMSet x [] = False
elemMSet y l@((a,n):t) | y == a = True
					   | otherwise = elemMSet y t
