{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.MessageTypeSpec ( spec ) where

import Taut.Prelude

import Data.Aeson             ( decode
                              , encode
                              )
import Taut.Types.MessageType ( MessageType
                              , fromText
                              )
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "MessageType" $ do
  context "serialization" $ do
    it "roundtrip" $ property $
      \mt -> (decode.encode) mt == Just (mt :: MessageType)
    it "specific example should be thus" $
      encode (fromText "abc") `shouldBe` "\"abc\""
