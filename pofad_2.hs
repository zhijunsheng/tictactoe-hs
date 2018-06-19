msc :: Ord a => [a] -> Int
msc xs = maximum [scount z zs | z:zs <- tails xs]
  where scount x = length . filter (x <)

tails :: [a] -> [[a]]
tails [] = []
tails l@(_:xs) = l:tails xs

tails' :: [a] -> [[a]]
tails' [] = []
tails' (x:xs) = (x:xs):tails' xs

