sum' :: Num a => [a] -> a
sum' [] = 0
sum' (n:ns) = n + sum' ns

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = [y | y <- xs, y <= x]
        larger = [y | y <- xs, y > x]

seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

