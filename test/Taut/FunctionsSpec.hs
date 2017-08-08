module Taut.Types.FunctionsSpec ( main, spec ) where

import qualified Data.Map                as Map
import           Infinity
import           Taut.Functions                          ( replyWindows )
import qualified Taut.Types.ChannelId    as ChannelId
import           Taut.Types.MessageEvent                 ( MessageEvent )
import qualified Taut.Types.MessageEvent as MessageEvent
import qualified Taut.Types.MessageType  as MessageType
import qualified Taut.Types.SubType      as SubType
import qualified Taut.Types.Timestamp    as Timestamp
import qualified Taut.Types.UserId       as UserId
import           Test.Hspec                              ( Spec
                                                         , context
                                                         , describe
                                                         , hspec
                                                         , it
                                                         , shouldBe
                                                         )
import           Test.QuickCheck                         ( property )

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "replyWindows" $ do
    context "given an empty list" $
      it "any size window returns an empty map" $ property $
        \n -> replyWindows n ([] :: [MessageEvent Text]) == Map.empty

    context "given a non-empty list" $
      it "a window of size 1 returns all immediate predecessors" $
        replyWindows 1 exampleMsgs `shouldBe` fixme
      where
        fixme = Map.empty

exampleMsgs :: [MessageEvent Text]
exampleMsgs = [msg1, msg2, msg3, msg4]
  where
    msg1 = MessageEvent.make cid1 edit editTs hid isS pay1 pin reac sub ts1 typ u1
    msg2 = MessageEvent.make cid2 edit editTs hid isS pay2 pin reac sub ts2 typ u2
    msg3 = MessageEvent.make cid3 edit editTs hid isS pay3 pin reac sub ts3 typ u3
    msg4 = MessageEvent.make cid4 edit editTs hid isS pay4 pin reac sub ts4 typ u4

    pay1 = "payload 1"
    pay2 = "payload 2"
    pay3 = "payload 3"
    pay4 = "payload 4"

    u1 = UserId.fromText "USER_A"
    u2 = UserId.fromText "USER_B"
    u3 = UserId.fromText "USER_C"
    u4 = UserId.fromText "USER_D"

    ts1 = Timestamp.fromSlackTimeText "100"
    ts2 = Timestamp.fromSlackTimeText "110"
    ts3 = Timestamp.fromSlackTimeText "200"
    ts4 = Timestamp.fromSlackTimeText "210"

    cid1 = ChannelId.fromText "CHAN_A"
    cid2 = cid1

    cid3 = ChannelId.fromText "CHAN_B"
    cid4 = cid3

    edit   = Nothing
    editTs = Nothing
    hid    = Nothing
    isS    = Nothing
    pin    = Nothing
    reac   = Nothing
    sub    = SubType.empty
    typ    = MessageType.message
