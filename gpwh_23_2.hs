{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.Semigroup

aWord :: T.Text
aWord = "Cheese"

main :: IO ()
main = do 
  print aWord

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

someText :: T.Text
someText = "Some\ntext for\t you"

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

combined :: String
combined = "some" ++ " " ++ "strings"

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some"," ","text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

