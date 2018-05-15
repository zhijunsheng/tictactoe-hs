data Tree a = Empty | Node (Tree a) a (Tree a) deriving Show

insert :: Ord a => Tree a -> a -> Tree a
insert Empty k = Node Empty k Empty
insert (Node l x r) k | k < x = Node (insert l k) x r
                      | otherwise = Node l x (insert r k)

toList :: Ord a => Tree a -> [a]
toList Empty = []
toList (Node l x r) = toList l ++ [x] ++ toList r

lookup' :: Ord a => Tree a -> a -> Tree a
lookup' Empty _ = Empty
lookup' t@(Node l k r) x | k == x = t
                         | x < k = lookup' l x
                         | otherwise = lookup' r x

bst = foldl insert Empty [4,3,1,2,8,7,16,10,9,14]
sorted = toList bst

