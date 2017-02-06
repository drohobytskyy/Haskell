nubL :: Eq a => [a] -> [a]
nubL [] = []
nubL l@(h:t) = h : nubL(filter(/= h) (l))
