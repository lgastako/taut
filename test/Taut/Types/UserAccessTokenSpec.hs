{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.UserAccessTokenSpec ( spec ) where

import Taut.Prelude

import Data.Aeson                 ( decode
                                  , encode
                                  )
import Taut.Types.UserAccessToken ( UserAccessToken
                                  , fromText
                                  )
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "UserAccessToken" $

  context "serialization" $ do

    it "should round trip" $ property $
      \accessToken -> (decode . encode) (accessToken :: UserAccessToken) == Just accessToken

    it "bot token should not work" $
      fromText "xoxbfoo" `shouldBe` Nothing

    it "example without xoxb perfix should fromText" $
      encode (fromText "non-xoxb-prefixed-string") `shouldBe` "\"non-xoxb-prefixed-string\""
