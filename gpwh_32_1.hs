import Control.Monad

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  value <- [1 .. n]
  return (2^value)

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  value <- [1 .. n]
  let powersOfTwo = 2^value
  let powersOfThree = 3^value
  return (powersOfTwo, powersOfThree)

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evenValue <- [2,4 .. n]
  oddValue <- [1,3 .. n]
  return (evenValue, oddValue)

evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard(even value)
  return value

filter' :: (a -> Bool) -> [a] -> [a]
filter' g xs = do
  value <- xs
  guard(g value)
  return value

