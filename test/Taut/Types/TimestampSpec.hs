{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.TimestampSpec ( main, spec ) where

import           Focus.Prelude

import qualified Taut.Types.Timestamp as Timestamp
import           Test.Hspec                        ( Spec
                                                   , hspec
                                                   , it
                                                   , shouldBe
                                                   )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "works on 0" $
    (show $ Timestamp.fromSlackTimeText "0")
      `shouldBe` ("Timestamp 1970-01-01 00:00:00 UTC" :: Text)

  it "works on negative numbers" $
    (show $ Timestamp.fromSlackTimeText "-1")
      `shouldBe` ("Timestamp 1969-12-31 23:59:59 UTC" :: Text)

  it "should work with 'modern' timestamps." $
    (show $ Timestamp.fromSlackTimeText "1455089423.133787")
      `shouldBe` ("Timestamp 2016-02-10 07:30:23.133786916732 UTC" :: Text)
