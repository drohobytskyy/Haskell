takeL :: Int -> [a] -> [a]
takeL n [] = []
takeL 0 l = []
takeL n (x:xs) = x : takeL (n-1) xs
