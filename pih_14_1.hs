foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' g v (x:xs) = g x (foldr' g v xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ v [] = v
foldl' g v (x:xs) = foldl' g (g v x) xs

