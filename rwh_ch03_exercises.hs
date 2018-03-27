length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

mean :: Fractional a => [a] -> a
mean [] = 0
mean xs = sum xs / fromIntegral (length' xs)

toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ reverse xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

