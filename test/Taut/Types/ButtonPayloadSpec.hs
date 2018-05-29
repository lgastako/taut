{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Taut.Types.ButtonPayloadSpec ( spec ) where

import Focus.Prelude

import Data.Aeson               ( decode
                                , encode
                                )
import Taut.Types.ButtonPayload ( ButtonPayload )
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "ButtonPayload" $ do
  it "should be roundtrippable" $ property $ do
    \payload -> (decode . encode $ payload) `shouldBe` Just (payload :: ButtonPayload)
