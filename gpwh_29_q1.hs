allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap g x = pure g <*> x

