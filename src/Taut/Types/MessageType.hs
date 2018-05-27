{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Taut.Types.MessageType
     ( MessageType
     , fromText
     , message
     , typeText
     , unMessageType
     ) where

import Taut.Prelude

import Control.Lens              ( Iso'
                                 , iso
                                 )
import Data.Csv                  ( ToField
                                 , toField
                                 )
import Data.Default              ( Default( def ) )
import Data.DeriveTH             ( derive
                                 , makeArbitrary
                                 )
import Test.QuickCheck           ( Arbitrary
                                 , arbitrary
                                 )
import Test.QuickCheck.Instances ()

newtype MessageType = MessageType { unMessageType :: Text }
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance ToField MessageType where
  toField (MessageType t) = encodeUtf8 t

instance Default MessageType where
  def = message

fromText :: Text -> MessageType
fromText = MessageType

typeText :: Iso' MessageType Text
typeText = iso unMessageType fromText

message :: MessageType
message = MessageType "message"

derive makeArbitrary ''MessageType
