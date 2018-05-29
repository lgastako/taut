{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.UserIdSpec ( spec ) where

import Taut.Prelude

import Data.Aeson        ( decode
                         , encode
                         )
import Taut.Types.UserId ( UserId
                         , fromText
                         )
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "UserId" $ do

  context "serialization" $ do

    it "roundtrip" $ property $
      \tid -> (decode.encode) tid == Just (tid :: UserId)

    it "this specific example should be thus" $ do
      (encode . fromText) "foo" `shouldBe` "\"foo\""
