import Data.Char

menor :: String -> String -> Bool
menor s1 s2 | (calc s1 - calc s2) < 0 = True
			| otherwise = False 

calc :: String ->Int
calc [] = 0
calc (h:t) = ord h + calc t
