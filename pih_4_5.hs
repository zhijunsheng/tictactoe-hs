true :: a -> b -> a
true = \x y -> x

false :: a -> b -> b
false = \x y -> y

not :: ((a1 -> b1 -> b1) -> (a2 -> b2 -> a2) -> t) -> t
not = \x -> x false true

--

add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> Int -> Int
add' = \x -> (\y -> x + y)

add'' :: Int -> (Int -> Int)
add'' = \x -> (\y -> x + y)

--

const' :: a -> b -> a
const' x _ = x

const'' :: a -> (b -> a)
const'' x = \_ -> x

--

odds :: Int -> [Int]
odds n = map f [0..n-1]
         where f x = 2*x + 1

odds' :: Int -> [Int]
odds' n = map (\x -> 2*x + 1) [0..n-1]

