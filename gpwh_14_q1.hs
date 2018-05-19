data Number = One | Two | Three deriving (Enum, Show) 

instance Eq Number where
  -- (==) :: a -> a -> Bool
  x == y = fromEnum x == fromEnum y

instance Ord Number where
  -- compare :: a -> a -> Ordering
  compare x y = compare (fromEnum x) (fromEnum y)

