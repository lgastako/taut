{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Taut.Types.Message.Attachment.Action.Confirm
       ( Confirm(Confirm)
       , text
       , title
       , dismissText
       , okText)
       where

import Focus.Prelude

import Control.Lens     ( makeLenses )
import Data.Aeson       ( FromJSON( parseJSON )
                        , ToJSON( toJSON )
                        , defaultOptions
                        , genericParseJSON
                        , genericToJSON
                        )
import Data.Aeson.Types ( Options( fieldLabelModifier
                                 , omitNothingFields
                                 )
                        , camelTo2
                        )
import Data.Text        ( Text )
import GHC.Generics     ( Generic )

data Confirm = Confirm
  { _title       :: Text
  , _text        :: Text
  , _okText      :: Text
  , _dismissText :: Text
  } deriving (Eq, Generic, Ord, Read, Show)

makeLenses ''Confirm

instance ToJSON Confirm where
  toJSON = genericToJSON customOptions

instance FromJSON Confirm where
  parseJSON = genericParseJSON customOptions

customOptions :: Options
customOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 1
  , omitNothingFields = True
  }
