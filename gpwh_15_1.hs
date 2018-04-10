data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show,Enum,Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where halfAlphabet = div alphabetSize 2
        offset = fromEnum c + halfAlphabet
        rotation = mod offset alphabetSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
  where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

message :: [FourLetterAlphabet]
message = [L1,L3,L4,L1,L1,L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
  where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rot4l = rotN alphaSize

data ThreeLetterApphabet = Alpha
                         | Beta
                         | Kappa deriving (Show,Enum,Bounded)

threeLetterMessage :: [ThreeLetterApphabet]
threeLetterMessage = [Alpha,Alpha,Beta,Alpha,Kappa]

threeLetterEncoder :: [ThreeLetterApphabet] -> [ThreeLetterApphabet]
threeLetterEncoder vals = map rot3l vals
  where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterApphabet)
        rot3l = rotN alphaSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where halfN = div n 2
        offset = if even n
                 then fromEnum c + halfN
                 else 1 + fromEnum c + halfN
        rotation = mod offset n

threeLetterDecoder :: [ThreeLetterApphabet] -> [ThreeLetterApphabet]
threeLetterDecoder vals = map rot3ldecoder vals
  where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterApphabet)
        rot3ldecoder = rotNdecoder alphaSize

rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where alphaSize = 1 + fromEnum (maxBound :: Char)
        rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where alphaSize = 1 + fromEnum (maxBound :: Char)
        rotCharDecoder = rotNdecoder alphaSize


