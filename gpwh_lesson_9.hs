map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if f x then x : rest else rest
  where rest = filter' f xs

remove :: (a -> Bool) -> [a] -> [a]
remove _ [] = []
remove f (x:xs) = if f x then rest else x:rest
  where rest = remove f xs

reverse' :: [a] -> [a]
reverse' = foldl (\x y -> y:x) []

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ x [] = x
foldl' f x (y:ys) = foldl' f (f x y) ys

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ x [] = x
foldr' f x (y:ys) = f y (foldr' f x ys)

