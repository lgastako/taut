{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.UserNameSpec ( spec ) where

import Taut.Prelude

import Data.Aeson          ( decode
                           , encode
                           )
import Taut.Types.UserName
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "UserName" $

  context "serialization" $ do

    it "should round trip" $ property $
      \uName -> (decode . encode) (uName :: UserName) == Just uName

    it "this example should serialize like so" $
      encode (fromText "whatever") `shouldBe` "\"whatever\""
