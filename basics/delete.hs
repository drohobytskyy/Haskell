delete :: Eq a => a -> [a] -> [a]
delete x l@(h:t) = if (h==x) then t else h:delete x t
