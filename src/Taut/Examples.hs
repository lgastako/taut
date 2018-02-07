{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Taut.Examples
       ( exampleMsg1
       ) where

import           Focus.Prelude

import           Data.Default                            ( def )
import           Taut
import qualified Taut.Types.ChannelId    as ChannelId
import qualified Taut.Types.MessageEvent as MessageEvent
import qualified Taut.Types.UserId       as UserId

exampleMsg1 :: MessageEvent Text
exampleMsg1 =
  MessageEvent.make
    (ChannelId.fromText "CH0PST1CK")
    Nothing
    Nothing
    Nothing
    Nothing
    "Example message #1."
    Nothing
    Nothing
    def
    def
    def
    (UserId.fromText "UDEADBEEF")
