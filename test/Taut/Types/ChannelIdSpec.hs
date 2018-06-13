{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.ChannelIdSpec ( spec ) where

import Taut.Prelude

import Data.Aeson           ( decode
                            , encode
                            )
import Taut.Types.ChannelId
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "ChannelId" $

  context "serialization" $ do

    it "should round trip" $ property $
      \cid -> (decode . encode) (cid :: ChannelId) == Just cid

    it "this example should serialize like so" $
      encode (fromText "C123") `shouldBe` "\"C123\""
