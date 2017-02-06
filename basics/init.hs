initL :: [a] -> [a]
initL [a] = []
initL (x:xs) = x : init xs
