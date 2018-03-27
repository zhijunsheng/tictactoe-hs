data BookInfo = Book Int String [String] 
                deriving (Show)

myInfo = Book 12345 "Algebra of Programming" ["Rechard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type Address = [String]

data Customer = Customer {
    customerID      :: CustomerID
  , customerName    :: String
  , customerAddress :: Address
  } deriving (Show)

