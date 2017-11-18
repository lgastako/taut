{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.Message.Attachment
       ( Attachment(Attachment)
       , fallback
       , color
       , pretext
       , attachmentType
       , actions
       , authorName
       , authorLink
       , authorIcon
       , callbackId
       , title
       , titleLink
       , text
       , fields
       , imageUrl
       , thumbUrl
       , footer
       , footerIcon
       , ts
       , Color(Good, Warning, Danger, ColorCode)
       , empty
       )
       where

import           Control.Lens                                  ( makeLenses )
import           Data.Aeson                                    ( FromJSON( parseJSON )
                                                               , ToJSON( toJSON )
                                                               , defaultOptions
                                                               , genericParseJSON
                                                               , genericToJSON
                                                               )
import qualified Data.Aeson                           as Aeson
import           Data.Aeson.Types                              ( Options( fieldLabelModifier
                                                                        , omitNothingFields
                                                                        )
                                                               , camelTo2
                                                               , typeMismatch
                                                               )
import           Data.Text                                     ( Text )
import qualified Data.Text                            as Text
import           GHC.Generics                                  ( Generic )
import           Taut.Types.Message.Attachment.Action          ( Action )
import           Taut.Types.Message.Attachment.Field           ( Field )
import           Taut.Types.Timestamp                          ( Timestamp )

data Color = Good
           | Warning
           | Danger
           | ColorCode Text
  deriving (Show)

instance ToJSON Color where
  toJSON (ColorCode code) = Aeson.String code
  toJSON x = Aeson.String . Text.toLower . Text.pack . show $ x

instance FromJSON Color where
  parseJSON (Aeson.String s) = case s of
    "good" -> return Good
    "warning" -> return Warning
    "danger" -> return Danger
    color -> return $ ColorCode color
  parseJSON invalid = typeMismatch "Color" invalid

data Attachment a = Attachment
  { _fallback       :: Maybe Text
  , _color          :: Maybe Color
  , _pretext        :: Maybe Text
  , _attachmentType :: Maybe Text
  , _actions        :: Maybe [Action]
  , _authorName     :: Maybe Text
  , _authorLink     :: Maybe Text
  , _authorIcon     :: Maybe Text
--  , _callbackId     :: Maybe CallbackId
  , _callbackId     :: Maybe a
  , _title          :: Maybe Text
  , _titleLink      :: Maybe Text
  , _text           :: Maybe Text
  , _fields         :: Maybe [Field]
  , _imageUrl       :: Maybe Text
  , _thumbUrl       :: Maybe Text
  , _footer         :: Maybe Text
  , _footerIcon     :: Maybe Text
  , _ts             :: Maybe Timestamp
  } deriving (Generic, Show)

makeLenses ''Attachment

instance ToJSON a => ToJSON (Attachment a) where
  toJSON = genericToJSON customOptions

instance FromJSON a => FromJSON (Attachment a) where
  parseJSON = genericParseJSON customOptions

customOptions :: Options
customOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 1
  , omitNothingFields = True
  }

empty :: Attachment a
empty = Attachment
  { _fallback = Nothing
  , _color = Nothing
  , _pretext = Nothing
  , _attachmentType = Nothing
  , _actions = Nothing
  , _authorName = Nothing
  , _authorLink = Nothing
  , _authorIcon = Nothing
  , _callbackId = Nothing
  , _title = Nothing
  , _titleLink = Nothing
  , _text = Nothing
  , _fields = Nothing
  , _imageUrl = Nothing
  , _thumbUrl = Nothing
  , _footer = Nothing
  , _footerIcon = Nothing
  , _ts = Nothing
  }
