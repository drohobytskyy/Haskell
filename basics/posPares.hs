posPares :: [a] -> [a]
posPares [] = []
posPares l@(h:t) | length (l) `mod`2 /= 0 = h : posPares t
				 | otherwise = posPares t
