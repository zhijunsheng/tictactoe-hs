data Box a = Box a deriving Show

instance Functor Box where
  fmap g (Box x) = Box (g x)

unwrap :: Box a -> a
unwrap (Box x) = x


