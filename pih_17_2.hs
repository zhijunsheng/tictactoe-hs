data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]

eval' :: Expr -> Stack -> Stack
eval' e s = eval e : s

push :: Int -> Stack -> Stack
push n s = n : s

add :: Stack -> Stack
add (m : n : s) = n+m : s

