foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)
_01 = foldr' (++) "" ["Bowen","Janna","Jolene","Sharon","Rachel","Kevin"]
_02 = foldr' (\x y -> length x + y) 0 ["Bowen","Janna","Jolene","Sharon","Rachel","Kevin"]
_03 = foldr' ((+) . length) 0 ["Bowen","Janna","Jolene","Sharon","Rachel","Kevin"]

sum' :: Num a => [a] -> a
sum' = foldr' (+) 0
_04 = sum' [1..100]

product' :: Num a => [a] -> a 
product' = foldr' (*) 1
_05 = product' [1..5]

or' :: [Bool] -> Bool
or' = foldr' (||) False
_06 = or' [False,False,True,False]
_07 = or' [False,False,False]

and' :: [Bool] -> Bool
and' = foldr' (&&) True
_08 = and' [True,True,False,True]
_09 = and' [True,True,True,True]

length' :: [a] -> Int
length' = foldr' (\_ n -> 1 + n) 0
_10 = length' []
_11 = length' [3,2,3,2]
_12 = length' "Donald"

reverse' :: [a] -> [a]
reverse' = foldr' (\x xs -> xs ++ [x]) []
_13 = reverse' [1,2,3]

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

length'' :: [a] -> Int
length'' = foldl' (\x _ -> x + 1) 0

reverse'' :: [a] -> [a]
reverse'' = foldl' (\xs x -> x:xs) []

