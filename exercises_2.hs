-- 5
init' :: [a] -> [a]
init' [] = error "empty list not allowed"
init' [x] = []
init' (x:xs) = [x] ++ init' xs 

init'' :: [a] -> [a]
init'' [] = error "empty list not allowed"
init'' [x] = []
init'' xs = reverse (tail (reverse xs))

-- 4
last' :: [a] -> a
last' [] = error "empty list not allowed"
last' [x] = x
last' (x:xs) = last' xs

last'' :: [a] -> a
last'' [] = error "empty list not allowed"
last'' xs = head (reverse xs)

-- 3
n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

