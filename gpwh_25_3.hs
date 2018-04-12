{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.IO as TIO

nagarjunaText :: T.Text
nagarjunaText = "नागर्जु"

nagarjunaSafe :: B.ByteString
nagarjunaSafe = E.encodeUtf8 nagarjunaText


