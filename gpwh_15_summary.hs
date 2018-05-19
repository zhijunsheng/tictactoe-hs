class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

data StreamCipher = SC Int

instance Cipher StreamCipher where
  encode (SC seed) text = applyOTP pad text where OTP pad = prngOTP seed
  decode (SC seed) text = applyOTP pad text where OTP pad = prngOTP seed

streamcipher_sample = decode (SC 12345) $ encode (SC 12345) "Haskell"

-- StreamCipher

prng :: Int -> Int -> Int -> Int -> Int
prng a b max seed = (a * seed + b) `mod` max

examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100

prngOTP :: Int -> OneTimePad
prngOTP seed = OTP $ map (bitsToChar.intToBits) (tail $ iterate examplePRNG seed)

-- OTP

type Bits = [Bool]

xor :: [Bool] -> [Bool] -> [Bool]
xor xs ys = map xorPair (zip xs ys)

xorPair :: (Bool,Bool) -> Bool
xorPair (x,y) = (x || y) && not (x && y)

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
  where bitList = applyOTP' pad plaintext

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair -> xor (fst pair) (snd pair))
                          (zip padBits plaintextBits)
  where padBits = map charToBits pad
        plaintextBits = map charToBits plaintext

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
               then intToBits' nextVal ++ [False]
               else intToBits' nextVal ++ [True]
  where remainder = mod n 2
        nextVal = div n 2

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ bits
  where bits = intToBits' n
        maxBits = length (intToBits' maxBound)
        missingBits = maxBits - (length bits)
        leadingFalses = take missingBits (cycle [False])

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
  where size = length bits
        indices = [size-1,size-2 .. 0]
        trueLocations = filter (\x -> fst x == True) (zip bits indices)

-- Rot

rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where alphaSize = 1 + fromEnum (maxBound :: Char)
        rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where alphaSize = 1 + fromEnum (maxBound :: Char)
        rotCharDecoder = rotNdecoder alphaSize

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where halfAlphabet = div alphabetSize 2
        offset = fromEnum c + halfAlphabet
        rotation = mod offset alphabetSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where halfN = div n 2
        offset = if even n
                 then fromEnum c + halfN
                 else 1 + fromEnum c + halfN
        rotation = mod offset n


