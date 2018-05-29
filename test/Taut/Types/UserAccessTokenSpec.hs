{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.UserAccessTokenSpec ( spec ) where

import Focus.Prelude

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

    it "this example should serialize like so" $
      encode (fromText "xoxbfoo") `shouldBe` "\"xoxbfoo\""

    it "example without xoxb perfix should not fromText" $
      fromText "non-xoxb-prefixed-string" `shouldBe` Nothing
