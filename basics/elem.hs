eleml :: Eq a => a -> [a] -> Bool
eleml _ [] = False
eleml a (h:t) | a == h = True
			  | otherwise =  eleml a t
