data Tree a = Empty | Node (Tree a) a (Tree a) deriving Show

insert :: Ord a => Tree a -> a -> Tree a
insert Empty k = Node Empty k Empty
insert (Node l x r) k | k < x = Node (insert l k) x r
                      | otherwise = Node l x (insert r k)

toList :: Ord a => Tree a -> [a]
toList Empty = []
toList (Node l x r) = toList l ++ [x] ++ toList r

bst = foldl' insert Empty [4,3,1,2,8,7,16,10,9,14]
sorted = toList bst

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

bst' = foldr' (flip' insert) Empty [4,3,1,2,8,7,16,10,9,14]
sorted' = toList bst'

