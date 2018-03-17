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

