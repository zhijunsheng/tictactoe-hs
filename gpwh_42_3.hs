import Data.STRef
import Control.Monad.ST

swapST :: (Int, Int) -> (Int, Int)
swapST (x, y) = runST $ do
  x' <- newSTRef x
  y' <- newSTRef y
  writeSTRef x' y
  writeSTRef y' x
  xfinal <- readSTRef x'
  yfinal <- readSTRef y'
  return (xfinal, yfinal)

