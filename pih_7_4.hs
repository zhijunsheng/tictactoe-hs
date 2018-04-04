sum' :: Num a => [a] -> a
sum' = sum'' 0
       where 
         sum'' v [] = v
         sum'' v (x:xs) = sum'' (v + x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

