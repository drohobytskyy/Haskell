enumfromthento :: Int -> Int -> Int -> [Int]
enumfromthento x y z = if (x <= z && y <= z) then x:enumfromthento (x + (y-1)) (y) (z)
											   else []

