andL :: [Bool] -> Bool
andL [] = True
andL l@(h:t) | h == False = False
			 | otherwise = andL t
