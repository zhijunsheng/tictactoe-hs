factors :: Int -> [Int]
factors n = [x | x <- [1..(ceiling . sqrt . fromIntegral) n] ++ [n], mod n x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k' == k]

