{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.MessageSpec ( spec ) where

import           Taut.Prelude

import           Data.Aeson                        ( decode
                                                   , encode
                                                   )
import qualified Taut.Types.ChannelId as ChannelId
import           Taut.Types.Message                ( Message( Message ) )
import qualified Taut.Types.UserName  as UserName
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "Message" $

  context "serialization" $ do

    it "roundtrip" $ property $
      \msg -> (decode.encode) msg == Just (msg :: Message)

    it "empty example should be thus" $ do
      let msg = Message
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing

      encode msg `shouldBe` "{}"

    it "specific example should be thus" $ do
      let msg = Message
            Nothing
            Nothing
            Nothing
            (Just $ ChannelId.fromText "cid")
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            (Just "this is the msg")
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            (Just $ UserName.fromText "username")

      encode msg `shouldBe` "{\"text\":\"this is the msg\",\"channel\":\"cid\",\"username\":\"username\"}"
