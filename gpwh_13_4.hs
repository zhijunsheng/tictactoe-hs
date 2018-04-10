class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe False = "A member of the Bool class, False is the opposite of True"
  describe True = "A member of the Bool class, True is the opposite of False"
 
instance Describable Int where
  describe x = "A member of the Int class, the number after " ++ show (x - 1) ++ " and before " ++ show (x + 1)

