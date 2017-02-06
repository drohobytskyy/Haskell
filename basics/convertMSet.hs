convertMSet :: [(a,Int)] -> [a]
convertMSet [] = []
convertMSet ((x,n):t) = replicate n x ++ convertMSet t



