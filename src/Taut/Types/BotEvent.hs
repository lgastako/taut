{-# LANGUAGE DeriveGeneric #-}

module Taut.Types.BotEvent
     ( BotEvent(..)
     ) where

import Data.Aeson           ( FromJSON
                            , ToJSON
                            , Value
                            )
import Data.Text            ( Text )
import GHC.Generics         ( Generic )
import Taut.Types.ChannelId ( ChannelId )
import Taut.Types.EditInfo  ( EditInfo )
import Taut.Types.Timestamp ( Timestamp )
import Taut.Types.UserId    ( UserId )

data BotEvent
  = Message ChannelId UserId Text Timestamp (Maybe Subtype) (Maybe EditInfo)
  | OtherEvent
  | UnknownEvent Value
  deriving (Eq, Generic, Read, Show)

type Subtype = Text

instance FromJSON BotEvent
instance ToJSON   BotEvent
