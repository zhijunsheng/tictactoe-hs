take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n - 1) xs
 
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (_:xs) = drop' (n - 1) xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

cycle' :: [a] -> [a]
cycle' [] = error "empty list not allowed"
cycle' xs = xs ++ cycle' xs

cycle'' :: [a] -> [a]
cycle'' [] = error "empty list not allowed"
cycle'' (x:xs) = x : cycle'' (xs ++ [x])

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fastFib :: Int -> Int -> Int -> Int
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y n = fastFib (y + x) x (n - 1)

