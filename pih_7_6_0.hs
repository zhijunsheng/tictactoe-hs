import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
  where weights = iterate (*2) 1

pi' = sum (take 100000 [4/(2*(fromIntegral x)+1)*(-1)^x | x <- [0.. ]])

repeat' :: a -> [a]
repeat' x = x : repeat' x

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = mod n 2 : int2bin (div n 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

