getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n - 1)

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = pure (:) <*> x <*> sequenceA' xs

getChars' :: Int -> IO String
getChars' n = sequenceA' $ replicate n getChar

