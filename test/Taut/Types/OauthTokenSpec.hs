{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.OauthTokenSpec ( spec ) where

import Taut.Prelude

import Data.Aeson            ( decode
                             , encode
                             )
import Taut.Types.OauthToken ( OauthToken
                             , fromText
                             )
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "OauthToken" $

  context "JSON serialization" $ do

    it "should roundtrip" $ property $
      \oauthToken -> (decode.encode) (oauthToken :: OauthToken) == Just oauthToken

    it "this example should serialize like so" $
      encode (fromText "foo") `shouldBe` "\"foo\""

