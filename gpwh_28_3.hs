data User = User
  { name :: String
  , gameId :: Int
  , score :: Int
  } deriving Show

serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGameId :: Maybe Int
serverGameId = Just 1729

serverScore :: Maybe Int
serverScore = Just 163

readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
  putStrLn "Enter a username, gamerId and score"
  user <- User <$> getLine <*> readInt <*> readInt
  print user


