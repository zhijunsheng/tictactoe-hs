data Box a = Box a deriving Show

boxMap :: (a -> b) -> Box a -> Box b
boxMap g (Box x) = Box (g x)

data Triple a = Triple a a a deriving Show

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap g (Triple x y z) = Triple (g x) (g y) (g z)

