helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = getLine >>= (\name -> (\statement -> putStrLn statement) (helloPerson name))

      
