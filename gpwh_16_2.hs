data Creator = AuthorCreator Author | ArtistCreator Artist

data Author = Author Name

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName 
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  | FirstNameWithTwoInits FirstName Char Char

data Artist = Person Name | Band String

hpLovecraft :: Creator 
hpLovecraft = AuthorCreator
                (Author
                  (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data Book = Book {
    author    :: Creator
  , isbn      :: String
  , bookTitle :: String
  , bookYear  :: Int
  , bookPrice :: Double
  }

data VinylRecord = VinylRecord {
    artist      :: Creator
  , recordTitle :: String
  , recordYear  :: Int
  , recordPrice :: Double
  }
  
data CollectibleToy = CollectibleToy {
    name        :: String
  , description :: String
  , toyPrice    :: Double
  }

data StoreItem = BookItem Book 
               | RecordItem VinylRecord
               | ToyItem CollectibleToy

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy

instance Show Creator where
  show (AuthorCreator (Author (Name f l)))  = "Author: " ++ f ++ " " ++ l
  show (AuthorCreator (Author (NameWithMiddle f m l))) = "Author: " ++ f ++ " " ++ m ++ " " ++ l
  show (ArtistCreator (Person (Name f l))) = "Artist: " ++ f ++ " " ++ l
  show (ArtistCreator (Person (NameWithMiddle f m l))) = "Artist: " ++ f ++ " " ++ m ++ " " ++ l
  show (ArtistCreator (Band s)) = "Band: " ++ s

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)

