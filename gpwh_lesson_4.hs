handleEven :: Integral a => a -> (a -> a) -> a
handleEven x f
  | even x = f x
  | otherwise = x

handleEven' :: Integral a => (a -> a) -> a -> a
handleEven' f x = if odd x then x else f x

