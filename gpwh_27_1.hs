successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

incMaybe' :: Maybe Int -> Maybe Int
incMaybe' = fmap (+1)

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just x) = Just (reverse x)
reverseMaybe Nothing = Nothing

reverseMaybe' :: Maybe String -> Maybe String
reverseMaybe' = fmap reverse

