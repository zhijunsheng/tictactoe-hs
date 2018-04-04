dot :: (b -> c) -> (a -> b) -> (a -> c)
dot f g x = f (g x) 

dot' :: (b -> c) -> (a -> b) -> (a -> c)
dot' f g = \x -> f (g x)

{-
composition is associative:
f . (g . h) = (f . g) . h

prove:

by definition,

f . (g . h) x = f ((g . h) x)
              = f (g (h x))

(f . g) . h x = (f . g) (h x)
              = f (g (h x))
-}

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

