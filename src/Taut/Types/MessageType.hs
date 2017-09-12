{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.MessageType
       ( MessageType
       , fromText
       , message
       , typeText
       ) where

import Control.Lens              ( Iso'
                                 , iso
                                 )
import Data.Aeson.TH             ( defaultOptions
                                 , deriveJSON
                                 )
import Data.Csv                  ( ToField
                                 , toField
                                 )
import Data.Default              ( Default( def ) )
import Data.DeriveTH             ( derive
                                 , makeArbitrary
                                 )
import Data.Text.Encoding        ( encodeUtf8 )
import Infinity
import Test.QuickCheck           ( Arbitrary
                                 , arbitrary
                                 )
import Test.QuickCheck.Instances ()

newtype MessageType = MessageType Text
  deriving (Eq, Ord, Read, Show)

instance ToField MessageType where
  toField (MessageType t) = encodeUtf8 t

instance Default MessageType where
  def = message

fromText :: Text -> MessageType
fromText = MessageType

toText :: MessageType -> Text
toText (MessageType t) = t

typeText :: Iso' MessageType Text
typeText = iso toText fromText

message :: MessageType
message = MessageType "message"

$(deriveJSON defaultOptions ''MessageType)

derive makeArbitrary ''MessageType
