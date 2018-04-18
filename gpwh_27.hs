maybeSquare :: Maybe Int -> Maybe String
maybeSquare = fmap (\x -> show (x^2) ++ "!") 

printInt :: Maybe String -> IO ()
printInt Nothing = putStrLn "value missing"
printInt (Just x) = putStrLn x

