enumfromto :: Int -> Int -> [Int]
enumfromto x y = if (x <= y) then x:enumfromto (x + 1) (y)
							 else []

