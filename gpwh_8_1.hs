take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' 0 x = x
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs

