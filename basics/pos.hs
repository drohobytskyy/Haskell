pos :: [a] -> Int -> a
pos (x:xs) n = if n == 0 then x else pos xs (n-1)
