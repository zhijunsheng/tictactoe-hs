import Data.Semigroup
import Data.List.Split

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' g v (x:xs) = g x (foldr' g v xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ v [] = v
foldl' g v (x:xs) = foldl' g (g v x) xs 

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedprobs
  where totalProbs = sum probs
        normalizedprobs = map (/totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine g xs ys = zipWith g newXs cycledYs
  where nToAdd = length ys
        repeatedXs = map (take nToAdd . repeat) xs
        newXs = mconcat repeatedXs
        cycledYs = cycle ys

combineEvents :: Events -> Events -> Events
combineEvents xs ys = cartCombine combiner xs ys
  where combiner = \x y -> mconcat [x,"-",y]

combineProbs :: Probs -> Probs -> Probs
combineProbs xs ys = cartCombine (*) xs ys

instance Semigroup PTable where
  (<>) pt (PTable [] []) = pt
  (<>) (PTable [] []) pt = pt
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where newEvents = combineEvents e1 e2
          newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

coin :: PTable 
coin = createPTable ["heads","tails"] [0.5,0.5]

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]

probsTotal = sum (map (read.(!!1).splitOn "|") (filter (not . null) (splitOn "\n" $ show (coin <> spinner))) :: [Double])

probsFromPTable :: PTable -> Probs
probsFromPTable (PTable x y) = y
probsTotal' = sum . probsFromPTable $ coin <> spinner

