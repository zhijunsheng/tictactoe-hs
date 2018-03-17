product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

rqsort :: Ord a => [a] -> [a]
rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
                where 
                  larger = [a | a <- xs, a >= x]
                  smaller = [a | a <- xs, a < x]

fqsort :: Ord a => [a] -> [a]
fqsort [] = []
fqsort (x:xs) = fqsort smaller ++ [x] ++ fqsort larger
                where smaller = [a | a <- xs, a < x]
                      larger = [a | a <- xs, a > x]

