allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM g x = x >>= \y -> return (g y)

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp f x = f >>= \g -> g <$> x

allApp' :: Monad m => m (a -> b) -> m a -> m b
allApp' f x = f >>= \g -> x >>= \y -> return (g y)

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just x) f = f x

