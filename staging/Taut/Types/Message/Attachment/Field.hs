{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Taut.Types.Message.Attachment.Field
       ( Field(Field)
       , title
       , value
       , short
       )
       where

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

data Field = Field
  { _title :: Text
  , _value :: Text
  , _short :: Bool
  } deriving (Generic, Show)

makeLenses ''Field

instance ToJSON Field where
  toJSON = genericToJSON customOptions

instance FromJSON Field where
  parseJSON = genericParseJSON customOptions


customOptions :: Options
customOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 1
  , omitNothingFields = True
  }
