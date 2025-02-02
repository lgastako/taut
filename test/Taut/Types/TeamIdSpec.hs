{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.TeamIdSpec ( spec ) where

import Taut.Prelude

import Data.Aeson        ( decode
                         , encode
                         )
import Taut.Types.TeamId ( TeamId
                         , fromText
                         )
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "TeamId" $

  context "serialization" $ do

    it "roundtrip" $ property $
      \tid -> (decode.encode) tid == Just (tid :: TeamId)

    it "this specific example should be thus" $
      (encode . fromText) "foo" `shouldBe` "\"foo\""
