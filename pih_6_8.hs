--1
fac :: Int -> Int
fac 0 = 1
fac n 
  | n < 0 = error "negative number not allowed"
  | otherwise = n * fac (n-1)

--2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

--3
exp' :: Num a => a -> Int -> a
exp' _ 0 = 1
exp' x n = x * exp' x (n-1)

--4
euclid :: Int -> Int -> Int
euclid a b 
  | a > b = euclid (a - b) b
  | a < b = euclid a (b - a)
  | otherwise = a

euclid' :: Int -> Int -> Int
euclid' a b
  | remainder == 0 = b
  | otherwise = euclid' b remainder
  where remainder = mod a b

--5
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs

init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs

--6
and' :: [Bool] -> Bool
and' [] = True
and' (False:xs) = False
and' (_:xs) = and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate (n-1) x

select' :: Int -> [a] -> a
select' _ [] = error "empty list not allowed"
select' 0 (x:_) = x
select' n (_:xs) = select' (n-1) xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
  | x == y = True
  | otherwise = elem' x ys

--7
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge ys (x:xs)

--8
halve :: [a] -> ([a],[a])
halve xs = (take h xs, drop h xs)
  where h = div (length xs) 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort first) (msort second)
  where first = fst halves
        second = snd halves
        halves = halve xs

--9
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

last' :: [a] -> a
last' [] = error "empty list not allowed"
last' [x] = x
last' (_:xs) = last' xs

