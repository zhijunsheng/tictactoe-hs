xorBool :: Bool -> Bool -> Bool
xorBool x y = (x || y) && (not (x && y))

xorPair :: (Bool,Bool) -> Bool
xorPair (x,y) = xorBool x y

xor :: [Bool] -> [Bool] -> [Bool]
xor xs ys = map xorPair (zip xs ys)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0) 
               then intToBits' nextN ++ [False]
               else intToBits' nextN ++ [True]
  where remainder = mod n 2
        nextN = div n 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ bits
  where bits = intToBits' n
        missingBits = maxBits - length bits
        leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
  where size = length bits
        indices = [size-1,size-2 .. 0]
        trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)


