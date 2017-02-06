initsL :: [a] ->[[a]]
initsL [] = [[]]
initsL l = initsL (init (l)) ++ [l]
