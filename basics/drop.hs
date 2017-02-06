dropL :: Int -> [a] -> [a]
dropL x [] = []
dropL 0 l = l
dropL n (x:xs) = dropL (n-1) xs
