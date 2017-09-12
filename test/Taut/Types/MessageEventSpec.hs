module Taut.Types.MessageEventSpec ( main , spec) where

import           Data.Aeson                               ( decode
                                                          , eitherDecode
                                                          , encode
                                                          )
import           Data.Aeson.Encode.Pretty                 ( confCompare
                                                          , defConfig
                                                          , encodePretty'
                                                          )
import           Data.Text                                ( Text )
import           Taut                                     ( MessageEvent )
import qualified Taut.Types.ChannelId     as ChannelId
import qualified Taut.Types.MessageEvent  as MessageEvent
import qualified Taut.Types.MessageType   as MessageType
import qualified Taut.Types.SubType       as SubType
import qualified Taut.Types.Timestamp     as Timestamp
import qualified Taut.Types.UserId        as UserId
import           Test.Hspec                               ( Spec
                                                          , context
                                                          , describe
                                                          , hspec
                                                          , it
                                                          , shouldBe
                                                          )
import           Test.QuickCheck                          ( property )

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "MessageEvent" $ do
  let msg1 = (MessageEvent.make
               (ChannelId.fromText "")
               Nothing
               Nothing
               (Just False)
               (Just False)
               ("0" :: Text)
               Nothing
               (Just [])
               (SubType.make "\186")
               (Timestamp.fromUTCTime . read $ "1864-05-09 09:54:38.732041347182 UTC")
               (MessageType.fromText "")
               (UserId.fromText ""))

      expectedPrettyMsg1 = "{\n    \"channel\": \"\",\n    \"edited\": null,\n    \"eventTs\": null,\n    \"hidden\": false,\n    \"isStarred\": false,\n    \"payload\": \"0\",\n    \"pinnedTo\": null,\n    \"reactions\": [],\n    \"subType\": \"\194\186\",\n    \"ts\": \"1864-05-09T09:54:38.732041347182Z\",\n    \"type\": \"\",\n    \"user\": \"\"\n}"

  context "serialization" $ do

    it "should encode field names properly" $ do
      encodePretty' (defConfig {confCompare=compare}) msg1
        `shouldBe`
        expectedPrettyMsg1

  context "deserialiation" $ do

    it "should decode an encoded msg properly" $ do
      eitherDecode expectedPrettyMsg1 `shouldBe` Right msg1

  context "roundtripping" $ do
    it "should be able to roundtrip this particular msg" $ do
      let rt = eitherDecode . encode $ msg1
      rt `shouldBe` (Right msg1)

    it "should roundtrip arbitrary messages successfully" $ property $
      roundTripSuccessfully

roundTripSuccessfully :: [MessageEvent Text] -> Bool
roundTripSuccessfully msgs = Just msgs == (decode . encode) msgs
