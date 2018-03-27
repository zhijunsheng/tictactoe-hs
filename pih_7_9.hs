-- 1
mapfilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapfilter f p xs = [f x | x <- xs, p x]

mapfilter' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapfilter' f p = map f . filter p

-- 2
all0 :: (a -> Bool) -> [a] -> Bool
all0 _ [] = True -- same as standard prelude
-- all' f [x] = f x -- works fine without the line above; not needed with it
all0 f (x:xs) = f x && all0 f xs
_200 = all0 even [1,2,3,4]
_201 = all0 even [2,4,6]
_202 = all0 even []

all1 :: (a -> Bool) -> [a] -> Bool
all1 f = foldr ((&&) . f) True
_203 = all1 even [1,2,3,4]
_204 = all1 even [2,4,6]
_205 = all1 even []

all2 :: (a -> Bool) -> [a] -> Bool
all2 f = foldl (\x y -> x && f y) True

all3 :: (a -> Bool) -> [a] -> Bool
all3 f = foldr (\x y -> f x && y) True

any0 :: (a -> Bool) -> [a] -> Bool
any0 f = foldr ((||) . f) False

any1 :: (a -> Bool) -> [a] -> Bool
any1 _ [] = False
any1 f (x:xs) = f x || any1 f xs

takeWhile0 :: (a -> Bool) -> [a] -> [a]
takeWhile0 f [] = []
takeWhile0 f (x:xs) = if f x 
                      then x : takeWhile0 f xs
                      else []

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 f = foldr (\x y -> if f x then x:y else []) []







