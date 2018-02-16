{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Taut.Types.Message.Attachment
       ( Attachment( Attachment )
       , Color( ColorCode
              , Good
              , Danger
              , Warning
              )
       , actions
       , attachmentType
       , authorIcon
       , authorLink
       , authorName
       , callbackId
       , color
       , fallback
       , fields
       , footer
       , footerIcon
       , imageUrl
       , pretext
       , text
       , thumbUrl
       , title
       , titleLink
       , ts
       ) where

import           Focus.Prelude

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
import           Data.Default                                  ( Default
                                                               , def
                                                               )
import qualified Data.Text                            as Text
import           Taut.Types.Message.Attachment.Action          ( Action )
import           Taut.Types.Message.Attachment.Field           ( Field )
import           Taut.Types.Timestamp                          ( Timestamp )

data Color
  = Good
  | Warning
  | Danger
  | ColorCode Text
  deriving (Eq, Generic, Ord, Read, Show)

instance ToJSON Color where
  toJSON (ColorCode code) = Aeson.String code
  toJSON x = Aeson.String . Text.toLower . Text.pack . show $ x

instance FromJSON Color where
  parseJSON (Aeson.String s) = case s of
    "good"    -> return Good
    "warning" -> return Warning
    "danger"  -> return Danger
    color     -> return $ ColorCode color
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
  } deriving (Eq, Generic, Ord, Read, Show)

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

instance Default (Attachment a) where
  def = Attachment
    { _fallback       = Nothing
    , _color          = Nothing
    , _pretext        = Nothing
    , _attachmentType = Nothing
    , _actions        = Nothing
    , _authorName     = Nothing
    , _authorLink     = Nothing
    , _authorIcon     = Nothing
    , _callbackId     = Nothing
    , _title          = Nothing
    , _titleLink      = Nothing
    , _text           = Nothing
    , _fields         = Nothing
    , _imageUrl       = Nothing
    , _thumbUrl       = Nothing
    , _footer         = Nothing
    , _footerIcon     = Nothing
    , _ts             = Nothing
    }
