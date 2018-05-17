type Name = (String, String)

data Sex = Female | Male deriving Show

data ABOType = A | B | AB | O deriving Show
data RhType = Pos | Neg deriving Show
data BloodType = BloodType ABOType RhType deriving Show

data Patient = Patient { name :: Name
                        ,sex :: Sex  
                        ,age :: Int
                        ,height :: Int
                        ,weight :: Int
                        ,bloodType :: BloodType } deriving Show

canDonateTo :: Patient -> Patient -> Bool
canDonateTo x y = case ((bloodType x), (bloodType y)) of
  (BloodType O _, _) -> True
  (_, BloodType AB _) -> True
  (BloodType A _, BloodType A _) -> True
  (BloodType B _, BloodType B _) -> True
  (_, _) -> False

p1 = Patient { name = ("Donald", "Sheng")
              ,sex = Male
              ,age = 52
              ,height = 173
              ,weight = 120
              ,bloodType = BloodType A Neg }

p2 = Patient { name = ("Cindy", "Zhang")
              ,sex = Male
              ,age = 52
              ,height = 173
              ,weight = 120
              ,bloodType = BloodType AB Neg }

