import Data.Semigroup

myAny :: (a -> Bool) -> [a] -> Bool
myAny x = (foldr (||) False) . (map x)

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' x = (foldl (||) False) . (map x)

data Color = Red 
           | Yellow
           | Blue
           | Green
           | Purple
           | Orange
           | Brown deriving (Show,Eq)

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b
    | a == b = a
    | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
    | all (`elem` [Yellow,Blue,Green]) [a,b] = Green
    | all (`elem` [Yellow,Red,Orange]) [a,b] = Orange
    | otherwise = Brown

instance Semigroup Integer where
  (<>) x y = x + y

