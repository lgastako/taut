{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.BotAccessTokenSpec ( spec ) where

import Focus.Prelude

import Data.Aeson                ( decode
                                 , encode
                                 )
import Taut.Types.BotAccessToken
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "BotAccessToken" $

  context "serialization" $ do

    it "should round trip" $ property $
      \accessToken -> (decode . encode) (accessToken :: BotAccessToken) == Just accessToken

    it "this example should serialize like so" $
      encode (fromText "xoxbfoo") `shouldBe` "\"xoxbfoo\""

    it "example without xoxb perfix should not fromText" $
      fromText "non-xoxb-prefixed-string" `shouldBe` Nothing
