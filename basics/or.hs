orL :: [Bool] -> Bool
orL [] = False
orL l@(h:t) | h == True = True
			| otherwise = orL t
