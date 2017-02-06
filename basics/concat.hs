concatL :: [[a]] -> [a]
concatL [] = []
concatL (h:t) = h ++ concat t
