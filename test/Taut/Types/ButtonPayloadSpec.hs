{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Taut.Types.ButtonPayloadSpec ( spec ) where

import Taut.Prelude

import Data.Aeson               ( decode
                                , encode
                                )
import Taut.Types.ButtonPayload ( ButtonPayload )
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "ButtonPayload" $
  it "should be roundtrippable" $ property $
    \payload -> (decode . encode $ payload) `shouldBe` Just (payload :: ButtonPayload)
