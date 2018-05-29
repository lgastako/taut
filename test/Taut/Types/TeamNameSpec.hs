{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.TeamNameSpec ( spec ) where

import Focus.Prelude

import Data.Aeson             ( decode
                              , encode
                              )
import Taut.Types.TeamName
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "TeamName" $

  context "serialization" $ do

    it "should round trip" $ property $
      \tname -> (decode . encode) (tname :: TeamName) == Just tname

    it "this example should serialize like so" $
      encode (fromText "whatever") `shouldBe` "\"whatever\""
