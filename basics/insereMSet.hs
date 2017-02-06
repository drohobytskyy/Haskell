insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet y ((z,n):t) | y == z = [(z,n+1)] ++ t
					   | otherwise = [(z,n)] ++ insereMSet y t
