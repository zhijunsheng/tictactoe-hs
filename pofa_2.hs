tails :: [a] -> [[a]]
tails [] = []
tails (x:xs) = (x:xs):tails xs

scount x xs = length (filter (x <) xs)

msc :: Ord a => [a] -> Int
msc xs = maximum [scount z zs | z:zs <- tails xs]

