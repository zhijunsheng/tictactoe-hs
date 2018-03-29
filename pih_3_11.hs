--1
-- [Char]
-- (Char,Char,Char)
-- [(Bool,Char)]
-- ([Bool],[Char])
-- [[a] -> [a]]

--2
bools :: [Bool]
bools = [False, True, False]

nums :: [[Int]]
nums = [[1,2,3],[4,5,6]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply f x = f x

--3
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double = (*2)

palindrome :: [Char] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

