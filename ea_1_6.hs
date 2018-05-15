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

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

min' :: Ord a => Tree a -> a
min' (Node Empty k _) = k
min' (Node l _ _) = min' l
min' Empty = error "empty tree"

max' :: Ord a => Tree a -> a
max' (Node _ k Empty) = k
max' (Node _ _ r) = max' r
max' Empty = error "empty tree"

delete :: Ord a => Tree a -> a -> Tree a
delete Empty _ = Empty
delete (Node l k r) x | x < k = Node (delete l x) k r
                      | x > k = Node l k (delete r x)
                      | isEmpty l = r
                      | isEmpty r = l
                      | otherwise = Node l k' (delete r k')
                          where k' = min' r

delete' :: Ord a => Tree a -> a -> Tree a
delete' Empty _ = Empty
delete' (Node l k r) x | x < k = Node (delete' l x) k r
                       | x > k = Node l k (delete' r x)
                       | isEmpty l = r
                       | isEmpty r = l
                       | otherwise = Node (delete' l k') k' r
                          where k' = max' l

bst = foldl insert Empty [4,3,1,2,8,7,16,10,9,14]
sorted = toList bst

