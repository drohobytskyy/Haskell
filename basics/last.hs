lastL :: [a] -> Maybe a
lastL [] = Nothing
lastL [x] = Just x
lastL (x:xs) = lastL xs
