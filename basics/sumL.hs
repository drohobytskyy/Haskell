sumL :: Num a => [a] -> a
sumL [] = 0
sumL l@(h:t) = h + sum t
