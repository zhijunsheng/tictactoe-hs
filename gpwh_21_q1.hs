import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

names :: Map.Map Int String
names = Map.fromList [(1, "Donald"), (2, "Cindy"), (3, "An")]

maybeMain :: Maybe String
maybeMain = do
  name <- Map.lookup 2 names
  let hello = helloPerson name
  return hello

