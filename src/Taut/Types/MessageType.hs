{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.MessageType
       ( MessageType
       , empty
       , fromText
       , message
       , typeText
       ) where

import Control.Lens       ( Iso'
                          , iso
                          )
import Data.Aeson.TH      ( defaultOptions
                          , deriveJSON
                          )
import Data.Csv           ( ToField
                          , toField
                          )
import Data.Text.Encoding ( encodeUtf8 )
import Infinity

newtype MessageType = MessageType Text
  deriving (Eq, Ord, Read, Show)

instance ToField MessageType where
  toField (MessageType t) = encodeUtf8 t

fromText :: Text -> MessageType
fromText = MessageType

toText :: MessageType -> Text
toText (MessageType t) = t

typeText :: Iso' MessageType Text
typeText = iso toText fromText

message :: MessageType
message = MessageType "message"

empty :: MessageType
empty = message

$(deriveJSON defaultOptions ''MessageType)
