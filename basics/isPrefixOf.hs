isprefixofL :: Eq a => [a] -> [a] -> Bool
isprefixofL [] _  = True
isprefixofL (h1:t1) (h2:t2) = if (h1 == h2) then isprefixofL t1 t2 else False
