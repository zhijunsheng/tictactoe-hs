safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))

betterSecond :: [a] -> Maybe a
betterSecond (_:x:_) = Just x
betterSecond _ = Nothing

second' :: [a] -> Maybe a
second' [] = Nothing
second' (x:[]) = Nothing
second' xs = Just (head (tail xs))

