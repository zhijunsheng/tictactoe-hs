--1
double :: Num a => a -> a
double x = x + x

--3
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

--4
qsort' :: Ord a => [a] -> [a]
qsort' [] = []
qsort' (x:xs) = qsort' larger ++ [x] ++ qsort' smaller
  where larger = [y | y <- xs, y >= x]
        smaller = [y | y <- xs, y < x]

--5
-- some elements may be lost
qsort'' :: Ord a => [a] -> [a]
qsort'' [] = []
qsort'' (x:xs) = qsort'' smaller ++ [x] ++ qsort'' larger 
  where smaller = [y | y <- xs, y < x]
        larger = [y | y <- xs, y > x]

