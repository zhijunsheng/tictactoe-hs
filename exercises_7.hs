filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p [] = []
filter'' p (x:xs) 
  | p x = x : filter'' p xs
  | otherwise = filter'' p xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs] 

map'' :: (a -> b) -> [a] -> [b]
map'' f [] = []
map'' f (x:xs) = f x : map'' f xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

{-
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f [] v = v
foldl' f (x:xs) v = f (foldl' f xs v) x
-}

