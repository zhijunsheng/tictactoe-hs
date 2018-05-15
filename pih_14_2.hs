import Data.Monoid
import Data.Foldable

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Foldable Tree where
  --fold :: Monoid a => Tree a -> a
  fold (Leaf x) = x
  fold (Node l r) = fold l `mappend` fold r

  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f v (Leaf x) = f x v
  foldr f v (Node l r) = foldr f (foldr f v r) l

  -- foldl :: (b -> a -> b) -> b -> Tree a -> b
  foldl f v (Leaf x) = f v x
  foldl f v (Node l r) = foldl f (foldl f v l) r

tree = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

treeSum = getSum $ foldMap (\x -> Sum x) tree
treeProduct = getProduct $ foldMap (\x -> Product x) tree

treeSum' = foldr (+) 0 tree
treeProduct' = foldr (*) 1 tree
treeProduct'' = foldl (*) 1 tree

