--1
sumsqr :: Int -> Int
sumsqr n = sum [x^2 | x <- [1..n]]

--2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

--3
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

--4
length :: [a] -> Int
length xs = sum [1 | _ <- xs]

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

--5
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

--6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

--7
comp :: [a] -> [a] -> [(a,a)]
--comp xs ys = [(x,y) | x <- xs, y <- ys]
comp xs ys = concat [[(x,y) | y <- ys] | x <- xs]

--8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k' == k]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

--9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

