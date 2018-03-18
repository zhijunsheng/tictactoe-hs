-- 2
bools :: [Bool]
bools = [True, False, False, False]

nums :: [[Int]]
nums = [[1,2,3], [5,5,5]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply a b = a b
{-

apply negate 5
apply reverse [1,2,3]

 -}
