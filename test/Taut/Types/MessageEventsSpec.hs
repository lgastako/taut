{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.MessageEventsSpec ( spec ) where

import           Focus.Prelude
import qualified Prelude                as P

import           Data.Aeson                  ( decode
                                             , encode
                                             )
import           Data.Default                ( def )
import           Taut.Types.MessageEvent     ( MessageEvent
                                             , make
                                             )
import           Taut.Types.MessageEvents
import           Test.Hspec
import           Test.QuickCheck
import           Taut.Types.MessageType as MessageType
import           Taut.Types.ChannelId                ( ChannelId )
import qualified Taut.Types.ChannelId   as ChannelId
import           Taut.Types.UserId                   ( UserId )
import qualified Taut.Types.UserId      as UserId

spec :: Spec
spec = describe "MessageEvents" $ do
  let now = P.read "Timestamp 2018-05-28 05:03:26.909048 UTC"

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
         now
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
         now
         (MessageType.fromText "mt2")
         uid2


  context "serialize" $ do

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
