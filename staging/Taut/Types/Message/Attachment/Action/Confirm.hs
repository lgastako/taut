{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.Message.Attachment.Action.Confirm
       ( Confirm( Confirm )
       , dismissText
       , okText
       , text
       , title
       ) where

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
  { _dismissText :: Text
  , _okText      :: Text
  , _title       :: Text
  , _text        :: Text
  } deriving (Generic, Show)

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
