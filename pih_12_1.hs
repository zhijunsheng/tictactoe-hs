inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

inc' = map (+1)
sqr' = map (^2)

class Functor' f where
  fmap :: (a -> b) -> f a -> f b

instance Functor' [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map

