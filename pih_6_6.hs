init' :: [a] -> [a]
init' [] = []
init' [_] = []
init' (x:xs) = x : init' xs

