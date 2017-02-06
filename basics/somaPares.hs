somaPares :: [Int] -> Int
somaPares [] = 0
somaPares (h:t) = if (h `mod`2 == 0) then h + somaPares t else somaPares t
