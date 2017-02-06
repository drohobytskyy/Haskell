reverseL :: [a] -> [a]
reverseL [] = []
reverseL [x] = [x]
reverseL l = last l:reverse (init l)
