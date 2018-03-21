gcd' :: Integral a => a -> a -> a
gcd' x 0 = x
gcd' x y = if x > y 
          then gcd' y (mod x y)
          else gcd' y x

gcd'' :: Integral a => a -> a -> a
gcd'' x y
  | x == 0 = y
  | y == 0 = x
  | x >= y = gcd'' (x - y) y
  | otherwise = gcd'' y x

gcd''' :: Integral a => a -> a -> a
gcd''' x 0 = x
gcd''' 0 x = x
gcd''' x y = if x >= y then gcd''' (x - y) y else gcd''' y x

gcd'''' :: Integral a => a -> a -> a
gcd'''' x 0 = x
gcd'''' x y = if remainder == 0 then y else gcd'''' y remainder
  where remainder = mod x y

sayAmount :: Integral a => a -> [Char]
sayAmount n = case n of 
  1 -> "one"
  2 -> "two"
  n -> "a bunch"

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n - 1) xs

