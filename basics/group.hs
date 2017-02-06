groupl :: Eq a => [a] -> [[a]]
groupl [] = [[]]
groupl (h:t) | h == head t = [[h]]
			 
