gcd' :: Int -> Int -> Int
gcd' x 0 = x
gcd' x y = gcd' y $ mod x y

gcd'' :: Integral a => a -> a -> a
gcd'' x y = if y == 0 then x else gcd'' y $ mod x y

lcm' :: Integral a => a -> a -> a
lcm' x y = x * y `div` gcd'' x y

