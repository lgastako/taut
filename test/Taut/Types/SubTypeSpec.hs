{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.SubTypeSpec ( spec ) where

import Focus.Prelude

import Data.Aeson         ( decode
                          , encode
                          )
import Taut.Types.SubType ( SubType
                          , make
                          )
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "SubType" $ do

  context "serialization" $ do

    it "roundtrip" $ property $
      \st -> (decode.encode) st == Just (st :: SubType)

    it "this specific example should be thus" $ do

      encode (make "foo") `shouldBe` "\"foo\""
