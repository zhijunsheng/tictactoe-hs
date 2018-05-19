import Data.Semigroup
import Data.List.Split

data Events = Events [String] deriving Show
data Probs = Probs [Double] deriving Show

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable (Events events) (Probs probs) = PTable (Events events) (Probs normalizedprobs)
  where totalProbs = sum probs
        normalizedprobs = map (/totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable (Events events) (Probs probs)) = mconcat pairs
    where pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine g xs ys = zipWith g newXs cycledYs
  where nToAdd = length ys
        repeatedXs = map (take nToAdd . repeat) xs
        newXs = mconcat repeatedXs
        cycledYs = cycle ys

combineEvents :: Events -> Events -> Events
combineEvents (Events xs) (Events ys) = Events $ cartCombine combiner xs ys
  where combiner = \x y -> mconcat [x,"-",y]

instance Semigroup Events where
  (<>) x (Events []) = x
  (<>) (Events []) x = x
  (<>) x y = combineEvents x y

instance Monoid Events where
  mempty = Events []
  mappend = (<>)

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs xs) (Probs ys) = Probs $ cartCombine (*) xs ys

instance Semigroup Probs where
  (<>) x (Probs []) = x
  (<>) (Probs []) x = x
  (<>) x y = combineProbs x y

instance Monoid Probs where
  mempty = Probs []
  mappend = (<>)

instance Semigroup PTable where
  (<>) pt (PTable (Events []) (Probs [])) = pt
  (<>) (PTable (Events []) (Probs [])) pt = pt
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where newEvents = e1 <> e2
          newProbs = p1 <> p2

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)

coin :: PTable 
coin = createPTable (Events ["heads","tails"]) (Probs [0.5,0.5])

spinner :: PTable
spinner = createPTable (Events ["red","blue","green"]) (Probs [0.1,0.2,0.7])

probsTotal = sum (map (read.(!!1).splitOn "|") (filter (not . null) (splitOn "\n" $ show (coin <> spinner))) :: [Double])

probsFromPTable :: PTable -> [Double]
probsFromPTable (PTable _ (Probs p)) = p
probsTotal' = sum . probsFromPTable $ coin <> spinner

