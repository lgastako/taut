{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.ChannelNameSpec ( spec ) where

import Taut.Prelude

import Data.Aeson             ( decode
                              , encode
                              )
import Taut.Types.ChannelName
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "ChannelName" $

  context "serialization" $ do

    it "should round trip" $ property $
      \chanName -> (decode . encode) (chanName :: ChannelName) == Just chanName

    it "this example should serialize like so" $
      encode (fromText "whatever") `shouldBe` "\"whatever\""
