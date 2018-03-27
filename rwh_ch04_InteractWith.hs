import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith myFunction = do
      (args, _) <- getArgs
      case args of 
        [input, output] -> interactWith myFunction input output
        _ -> putStrLn "error: exactly two arguments needed"

    myFunction = id

