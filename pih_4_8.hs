-- 1
halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
           where half = div (length xs) 2

--2
third :: [a] -> a
third = head . tail . tail

third' :: [a] -> a
third' xs = xs!!2

third'' :: [a] -> a
third'' (x0:x1:x2:xs) = x2

--3
safetail :: [a] -> [a]
safetail xs = if null xs 
              then []
              else tail xs

safetail' :: [a] -> [a]
safetail' xs
  | null xs = []
  | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (x:xs) = xs

--4
(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _ _ = False

(||) :: Bool -> Bool -> Bool
(||) False False = False
(||) _ _ = True

oo :: Bool -> Bool -> Bool
oo True False = True
oo False True = True
oo True True = True
oo False False = False

oo' :: Bool -> Bool -> Bool
oo' True _ = True
oo' _ True = True
oo' _ _ = False


oo'' :: Bool -> Bool -> Bool
oo'' False b = b
oo'' True _ = True

--5
aa :: Bool -> Bool -> Bool
aa x y = if x 
         then
          if y 
          then True
          else False
         else False

--6
aa' :: Bool -> Bool -> Bool
aa' x y = if x
          then y
          else False

--7
mult :: Int -> Int -> Int -> Int 
mult x y z = x*y*z

mult' :: Int -> (Int -> (Int -> Int))
mult' = \x -> (\y -> (\z -> x*y*z))

--8
luhnDouble :: Int -> Int
luhnDouble x = if 2*x > 9 
               then 2*x - 9
               else 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = mod (luhnDouble a + b + luhnDouble c + d) 10 == 0



