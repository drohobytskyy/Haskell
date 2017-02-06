removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = [] 
removeMSet z ((a,n):t) | z == a = if n == 1 then t else [(a,n-1)] ++ t
					   | otherwise = [(a,n)] ++ removeMSet z t
