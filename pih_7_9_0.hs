func_7_9_1 :: (a -> Bool) -> (a -> b) -> [a] -> [b]
func_7_9_1 p f xs = [f x | x <- xs, p x]

func_7_9_1' :: (a -> Bool) -> (a -> b) -> [a] -> [b]
func_7_9_1' p f = map f . filter p

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) = p x && all' p xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

all'' :: (a -> Bool) -> [a] -> Bool
all'' f = foldr' ((&&) . f) True 

all''' :: (a -> Bool) -> [a] -> Bool
all''' f = foldr' (\x y -> f x && y) True

--all'''' :: (a -> Bool) -> [a] -> Bool
--all'''' f = foldl' (f . (&&)) True

all''''' :: (a -> Bool) -> [a] -> Bool
all''''' f = foldl' (\x y -> x && f y) True

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) = f x || any' f xs

any'' :: (a -> Bool) -> [a] -> Bool
any'' f = foldr' ((||) . f) False

any''' :: (a -> Bool) -> [a] -> Bool
any''' f = foldr' (\x y -> f x || y) False

dot :: (b -> c) -> (a -> b) -> (a -> c)
dot f g = \x -> f (g x)

revertdot :: (a -> b) -> (b -> c) -> (a -> c)
revertdot f g = \x -> g (f x)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' p = foldr' (\x y -> if p x then x : y else []) []

--takeWhile''' :: (a -> Bool) -> [a] -> [a]
--takeWhile''' p = foldl' (\x y -> if p y then x ++ [y] else x) []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) 
  | p x = dropWhile' p xs
  | otherwise = x:xs

--3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr' (\x y -> f x : y) [] 

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr' ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr' (\x y -> if p x then x:y else y) [] 

--4
dec2int :: [Int] -> Int
dec2int = (`div` 10) . foldl' (\x y -> (x+y)*10) 0

dec2int' :: [Int] -> Int
dec2int' = foldl' (\x y -> x*10 + y) 0


--5
curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x,y) -> f x y


