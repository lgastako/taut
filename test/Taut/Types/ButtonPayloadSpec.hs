{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Taut.Types.ButtonPayloadSpec ( main, spec ) where

import Data.Aeson                         ( decode
                                          , encode
                                          )
import Focus.Prelude
import Taut.Types.ButtonPayload           ( ButtonPayload )
import Taut.Types.ButtonPayload.Arbitrary ()
import Test.Hspec                         ( Spec
                                          , describe
                                          , hspec
                                          , it
                                          , shouldBe
                                          )
import Test.QuickCheck                    ( property )

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ButtonPayload" $ do
  it "should be roundtrippable" $ property $ do
    \payload -> (decode . encode $ payload) `shouldBe` Just (payload :: ButtonPayload ())
