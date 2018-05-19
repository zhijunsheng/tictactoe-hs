data FiveSidedDie = FS1 | FS2 | FS3 | FS4 | FS5 deriving (Enum, Eq, Show)

data SixSidedDie = SS1 | SS2 | SS3 | SS4 | SS5 | SS6 deriving (Enum, Eq, Show)

class (Eq a, Enum a) => Die a where
  roll :: Int -> a

instance Die FiveSidedDie where
  roll n = toEnum (n `mod` 5)

instance Die SixSidedDie where
  roll n = toEnum (n `mod` 6)

roll_5_7 = roll 7 :: FiveSidedDie
-- FS3

roll_6_7 = roll 7 :: SixSidedDie
-- SS2

