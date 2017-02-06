unwordsL :: [String] -> String
unwordsL l@(h:t) | length t > 0 = h ++ " " ++ unwordsL t
				 | otherwise = h
