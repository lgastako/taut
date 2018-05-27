{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Taut.Types.Message.Attachment.Field
     ( Field( Field )
     , short
     , title
     , value
     ) where

import Taut.Prelude

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

data Field = Field
  { _short :: Bool
  , _title :: Text
  , _value :: Text
  } deriving (Eq, Generic, Ord, Read, Show)

makeLenses ''Field

instance FromJSON Field where
  parseJSON = genericParseJSON fieldOptions

instance ToJSON Field where
  toJSON = genericToJSON fieldOptions

fieldOptions :: Options
fieldOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 1
  , omitNothingFields  = True
  }
