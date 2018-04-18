echoVerbose :: IO ()
echoVerbose = putStrLn "Enter a string and we'll echo it!" >> getLine >>= putStrLn

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >> getLine >>= (\name -> return (nameStatement name)) >>= putStrLn

