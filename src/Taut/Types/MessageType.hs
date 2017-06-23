module Taut.Types.MessageType
       ( MessageType
       , make
       , message
       ) where

import Data.Text ( Text )

newtype MessageType = MessageType Text
  deriving (Eq, Ord, Read, Show)

make :: Text -> MessageType
make = MessageType

message :: MessageType
message = MessageType "message"
