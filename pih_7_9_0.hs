import Data.Char

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

--6
int2bin :: Int -> [Int]
int2bin 0 = []
int2bin x = (int2bin (div x 2)) ++ [mod x 2]

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x 
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bin' :: Int -> [Int]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

map''' :: (a -> b) -> [a] -> [b]
map''' f = unfold null (f . head) tail

chop8 :: [Int] -> [[Int]]
chop8 = unfold null (take 8) (drop 8)

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) id f

--7
type Bit = Int
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)
addparity :: [Bit] -> [Bit]
addparity xs = if even (sum xs) then xs ++ [0] else xs ++ [1]
encode :: String -> [Bit]
encode = concat . map''' (addparity . make8 . int2bin' . ord) 
bin2int :: [Bit] -> Int
bin2int = sum . zipWith (*) (iterate' (*2) 1) 
chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null (take 9) (drop 9)
checkparity :: [Bit] -> [Bit]
checkparity xs = if even (sum xs) then take 8 xs else error "failed with parity"
decode :: [Bit] -> String
decode = map''' chr . map''' bin2int . map''' checkparity . chop9
channel :: [Bit] -> [Bit]
channel = id
transmit :: String -> String
transmit = decode . channel . encode

--9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = [if even i then f x else g x | (i,x) <- zip [0..] xs]

--10
luhnDouble :: Int -> Int
luhnDouble x = if 2*x > 9 then 2*x - 9 else 2*x

luhn :: [Int] -> Bool
luhn xs = sum (altMap (luhnDouble) (id) xs) `mod` 10 == 0


