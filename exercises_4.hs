-- 1
halve :: [a] -> ([a], [a])
halve xs 
  | not (even (length xs))    = error "list length not even"
  | otherwise = (take x xs, drop x xs)
                where 
                  x = div (length xs) 2

--2
third :: [a] -> a
third xs = if length xs < 3 then error "list too short" else
            head (tail (tail xs))

third' :: [a] -> a
third' xs 
  | length xs < 3 = error "list too short"
  | otherwise     = xs !! 2

third'' :: [a] -> a
third'' (x:y:z:rest) = z
third'' _ = error "list too short"

-- 3
safetail :: [a] -> [a]
safetail xs = if null xs then xs else tail xs

safetail' :: [a] -> [a]
safetail' xs 
  | null xs = xs
  | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

--7
mult :: Int -> Int -> Int -> Int
mult = (\x -> (\y -> (\z -> x*y*z)))
