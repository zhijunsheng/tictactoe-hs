data Expr = Val Int | Div Expr Expr

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just $ div x y

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = eval x >>= \n -> 
                 eval y >>= \m -> 
                 safeDiv n m

eval' :: Expr -> Maybe Int
eval' (Val n) = Just n
eval' (Div x y) = do
  n <- eval' x
  m <- eval' y
  safeDiv n m

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = xs >>= \x ->
              ys >>= \y ->
              return (x,y)

pairs' :: [a] -> [b] -> [(a,b)]
pairs' xs ys = do
                x <- xs
                y <- ys
                pure (x,y)

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g (S st) = S (\s -> let (x, s') = st s in (g x, s'))

S fst0 = S (\x -> ("Donald", x + 2))
S fst1 = ('=':) <$> S fst0
S fst2 = reverse <$> S fst0

S fst3 = S (\x -> ([1,2,3], x + 1))
S fst4 = sum <$> S fst3
S fst5 = map (^2) <$> S fst3


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'') where (l', n')  = rlabel l n
                                              (r', n'') = rlabel r n'

