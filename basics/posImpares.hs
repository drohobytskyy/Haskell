posImp :: [a] -> [a]
posImp [] = []
posImp l@(h:t) | length (l) `mod` 2 == 0 = h : posImp t
			   | otherwise = posImp t
