cycleSucc :: (Bounded a, Enum a, Ord a) => a -> a
cycleSucc x = if x < maxBound then succ x else minBound

