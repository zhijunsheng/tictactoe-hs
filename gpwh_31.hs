maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM x = x >>= \y -> pure (max (fst y) (snd y))

