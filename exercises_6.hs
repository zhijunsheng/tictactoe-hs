fac :: Int -> Int
fac n = product [1..n]

fac' :: Integral a => a -> a
fac' n = product [1..n]

fac'' :: Integral a => a -> a
fac'' 0 = 1
fac'' n = n * fac'' (n - 1)

product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product' ns

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y = x:y:ys
  | otherwise = y:insert x ys

