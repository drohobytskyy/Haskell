elemIndicesL :: Eq a => a-> [a] -> [Int]
elemIndicesL _ [] = []
elemIndicesL x l = aux (x) (0) (l)

aux (elemento) (contador) [] = []
aux (elemento) (contador) (lista) = if (elemento == head lista) then contador : aux (elemento) (contador+1) (tail lista) else aux (elemento) (contador+1) (tail lista)

