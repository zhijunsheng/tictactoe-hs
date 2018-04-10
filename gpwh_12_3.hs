data Patient = Patient Name Sex Int Int Int BloodType

type FirstName = String
type MiddleName = String
type LastName = String

data Name = Name FirstName LastName 
  | NameWithMiddle FirstName MiddleName LastName

data Sex = Male | Female

data ABOType = A | B | AB | O
data RhType = Pos | Neg

data BloodType = BloodType ABOType RhType

getName :: Patient -> Name 
getName (Patient n _ _ _ _ _) = n

getAge :: Patient -> Int
getAge (Patient _ _ a _ _ _) = a

data Patient' = Patient' { name :: Name
                         , sex :: Sex
                         , age :: Int
                         , height :: Int
                         , weight :: Int
                         , bloodType :: BloodType }

canDonate :: BloodType -> BloodType -> Bool
canDonate (BloodType O _) (BloodType _ _) = True
canDonate (BloodType _ _) (BloodType AB _) = True
canDonate (BloodType A _) (BloodType A _) = True
canDonate (BloodType B _) (BloodType B _) = True
canDonate _ _ = False

canDonateTo :: Patient' -> Patient' -> Bool
canDonateTo p0 p1 = canDonate (bloodType p0) (bloodType p1)

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showRh :: RhType -> String
showRh Neg = "-"
showRh Pos = "+"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh 

patientSummary :: Patient' -> IO ()
patientSummary p = putStrLn ("****************" ++ "\n" ++ "Patient Name: " ++ showName (name p) ++ "\n" ++ "Sex: " ++ showSex (sex p) ++ "\n" ++ "Age: " ++ show (age p) ++ "\n" ++ "Height: " ++ show (height p) ++ " in.\n" ++ "Weight: " ++ show (weight p) ++ " lbs.\n" ++ "Blood Type: " ++ showBloodType (bloodType p) ++ "\n****************\n")


