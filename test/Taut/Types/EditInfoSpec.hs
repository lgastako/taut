{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.EditInfoSpec ( spec ) where

import           Taut.Prelude                      hiding ( from )

import           Data.Aeson                               ( decode
                                                          , encode
                                                          )
import           Taut.Types.EditInfo
import qualified Taut.Types.Timestamp as Timestamp
import qualified Taut.Types.UserId    as UserId
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "EditInfo" $

  context "serialization" $ do

    it "should round trip" $ property $
      \ei -> (decode . encode) (ei :: EditInfo) == Just ei

    it "this example should serialize like so" $ do
      let ts' = Timestamp.fromSlackTimeText "0"
          uid = UserId.fromText "U123"

      encode (from (ts', uid)) `shouldBe`
        "{\"_ts\":\"1970-01-01T00:00:00Z\",\"_user\":\"U123\"}"
