{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

sampleString :: String
sampleString = BC.unpack sampleBytes

asciiCharToInt :: BC.ByteString -> Int
asciiCharToInt = read . BC.unpack

bcInt :: BC.ByteString
bcInt = "1729"


