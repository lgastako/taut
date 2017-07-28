module Taut.Types.TimestampSpec ( main, spec ) where

import           Infinity
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
    (tshow $ Timestamp.fromSlackTimeText "0")
      `shouldBe` "Timestamp 1970-01-01 00:00:00 UTC"

  it "works on negative numbers" $
    (tshow $ Timestamp.fromSlackTimeText "-1")
      `shouldBe` "Timestamp 1969-12-31 23:59:59 UTC"

  it "should work with 'modern' timestamps." $
    (tshow $ Timestamp.fromSlackTimeText "1455089423.133787")
      `shouldBe` "Timestamp 2016-02-10 07:30:23.133786916732 UTC"
