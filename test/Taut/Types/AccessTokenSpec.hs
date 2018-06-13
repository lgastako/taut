{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.AccessTokenSpec ( spec ) where

import Taut.Prelude

import Data.Aeson             ( decode
                              , encode
                              )
import Taut.Types.AccessToken
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "AnyAccessToken" $

  context "serialization" $ do

    it "should round trip" $ property $
      \accessToken -> (decode . encode) (accessToken :: AnyAccessToken) == Just accessToken

    it "this example should serialize like so" $
      encode (fromText "foo") `shouldBe` "\"foo\""
