tailsL :: [a] -> [[a]]
tailsL [] = []
tailsL l = l : tailsL(tail l)
