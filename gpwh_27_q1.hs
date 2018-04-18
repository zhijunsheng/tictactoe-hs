data Box a = Box a deriving Show

instance Functor Box where
  fmap g (Box x) = Box (g x)

morePresents :: Int -> Box a -> Box [a]
morePresents n (Box x) = take n . repeat <$> Box x 

morePresents' :: Int -> Box a -> Box [a]
morePresents' n = fmap (take n . repeat)



