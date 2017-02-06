productL :: Num a => [a] -> a
productL [] = 1
productL l@(h:t) = h * productL t
