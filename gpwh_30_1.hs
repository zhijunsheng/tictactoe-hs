import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList 
  [ (1, "nYarlathoTep")
  , (2, "KINGinYELLOW")
  , (3, "dagon1997")
  , (4, "rcarter1910")
  , (5, "xCTHULHUx")
  , (6, "YOGSOTHOTH")]

creditDB :: Map.Map UserName PlayerCredits
creditDB = Map.fromList
  [ ("nYarlathoTep", 2000)
  , ("KINGinYELLOW", 15000)
  , ("dagon1997", 300)
  , ("rcarter1910", 12)
  , ("xCTHULHUx", 50000)
  , ("YOGSOTHOTH", 150000)]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditDB

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

echo :: IO ()
echo = getLine >>= putStrLn

main :: IO ()
main = echo

