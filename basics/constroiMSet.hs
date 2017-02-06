constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet x@(h:t) = (h,length(filter(==h) (x))) : constroiMSet (filter(/= h) (x))
