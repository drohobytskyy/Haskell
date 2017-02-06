lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,n):t) = n + lengthMSet t
