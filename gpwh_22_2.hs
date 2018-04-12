import Data.List.Split

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

replicate'' :: Int -> a -> [a]
replicate'' n x = map (\_ -> x) [1..n]

toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum numbers)

