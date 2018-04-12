map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' g (x:xs) = g x : map g xs

maybeMap :: (Maybe a -> Maybe b) -> [Maybe a] -> [Maybe b]
maybeMap _ [] = []
maybeMap g (x:xs) = g x : maybeMap g xs

maybeEven :: Maybe Int -> Maybe Bool
maybeEven Nothing = Nothing
maybeEven (Just x) = Just (even x)

