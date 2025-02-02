{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.MessageEventsSpec ( spec ) where

import           Taut.Prelude

import           Data.Aeson                              ( decode
                                                         , encode
                                                         )
import           Data.Default                            ( def )
import           Taut.Types.ChannelId                    ( ChannelId )
import qualified Taut.Types.ChannelId     as ChannelId
import           Taut.Types.MessageEvent                 ( MessageEvent
                                                         , make
                                                         )
import           Taut.Types.MessageEvents
import           Taut.Types.MessageType   as MessageType
import qualified Taut.Types.Timestamp     as Timestamp
import           Taut.Types.UserId                       ( UserId )
import qualified Taut.Types.UserId        as UserId
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "MessageEvents" $ do
  let Just now = readMaybe "2018-05-28 05:03:26.909048 UTC"
      ts = Timestamp.fromUTCTime now

      cid1 :: ChannelId
      cid1 = ChannelId.fromText "C123"

      cid2 :: ChannelId
      cid2 = ChannelId.fromText "C234"

      uid1 :: UserId
      uid1 = UserId.fromText "U123"

      uid2 :: UserId
      uid2 = UserId.fromText "U234"

      example1 = make
         cid1
         Nothing
         Nothing
         Nothing
         Nothing
         "hello world"
         Nothing
         Nothing
         def
         ts
         (MessageType.fromText "mt1")
         uid1

      example2 = make
         cid2
         Nothing
         Nothing
         Nothing
         Nothing
         "what up dude"
         Nothing
         Nothing
         def
         ts
         (MessageType.fromText "mt2")
         uid2


  context "serialize" $

    it "should roundtrip" $ property $
      \me -> (decode.encode) me == Just (me :: MessageEvent Text)

  -- context "onlyMessages" $

    -- it "should something" $ property $
    --   \me -> onlyMessages (me :: [MessageEvent ()]) == []

  context "toCSV" $ do

    it "empty MessageEvents should work thusly" $ do

      let mes :: [MessageEvent Text]
          mes = []

      toCSV mes `shouldBe` "channelId,userId,payload,type,subType,ts\r\n"

    it "non-empty MessageEvents should work thusly" $ do

      let mes :: [MessageEvent Text]
          mes = [example1, example2]

      toCSV mes `shouldBe` "channelId,userId,payload,type,subType,ts\r\nC123,U123,\"hello world\",mt1,,1527483807\r\nC234,U234,\"what up dude\",mt2,,1527483807\r\n"
