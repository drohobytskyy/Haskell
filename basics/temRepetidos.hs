temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) | aux h t = True
				   | otherwise = temRepetidos t

aux a [] = False
aux a (h:t) | a == h = True
			| otherwise = aux a t
