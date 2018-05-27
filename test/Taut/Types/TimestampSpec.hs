{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.TimestampSpec ( main, spec ) where

import Taut.Prelude

import Taut.Types.Timestamp ( fromSlackTimeText )
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Timestamp.fromSlackTimeText" $ do

  let roundtrip :: Text -> Text
      roundtrip = show . fromSlackTimeText

  it "works on 0" $
    roundtrip "0" `shouldBe`
      "Timestamp {unTimestamp = 1970-01-01 00:00:00 UTC}"

  it "works on negative numbers" $
    roundtrip "-1" `shouldBe`
      "Timestamp {unTimestamp = 1969-12-31 23:59:59 UTC}"

  it "should work with 'modern' timestamps." $
    roundtrip "1455089423.133787" `shouldBe`
      "Timestamp {unTimestamp = 2016-02-10 07:30:23.133786916732 UTC}"
