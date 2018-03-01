module Taut.Types.BotEvent
     ( BotEvent(..)
     ) where

import Data.Aeson           ( Value )
import Data.Text            ( Text )
import Taut.Types.ChannelId ( ChannelId )
import Taut.Types.EditInfo  ( EditInfo )
import Taut.Types.Timestamp ( Timestamp )
import Taut.Types.UserId    ( UserId )

data BotEvent
  = Message ChannelId UserId Text Timestamp (Maybe Subtype) (Maybe EditInfo)
  | OtherEvent
  | UnknownEvent Value
  deriving (Eq, Read, Show)

type Subtype = Text
