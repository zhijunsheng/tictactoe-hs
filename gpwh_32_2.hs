import Control.Monad

evenSquares :: [Int]
evenSquares = do
  n <- [0 .. 9]
  let nSquared = n^2
  guard (even nSquared)
  return nSquared

