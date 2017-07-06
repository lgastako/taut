module Taut.Examples
       ( exampleMsg1
       ) where

import           Infinity
import           Taut
import qualified Taut.Types.ChannelId    as ChannelId
import qualified Taut.Types.MessageEvent as MessageEvent
import qualified Taut.Types.MessageType  as MessageType
import qualified Taut.Types.SubType      as SubType
import qualified Taut.Types.Timestamp    as Timestamp
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
    SubType.empty
    Timestamp.empty
    MessageType.empty
    (UserId.fromText "UDEADBEEF")
