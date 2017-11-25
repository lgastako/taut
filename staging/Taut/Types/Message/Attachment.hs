{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.Message.Attachment
       ( Attachment( Attachment )
       , Color( ColorCode
              , Danger
              , Good
              , Warning
              )
       , actions
       , attachmentType
       , authorName
       , authorLink
       , authorIcon
       , callbackId
       , color
       , empty
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

data Color
  = ColorCode Text
  | Danger
  | Good
  | Warning
  deriving (Eq, Ord, Read, Show)

instance FromJSON Color where
  parseJSON (Aeson.String s) = return $ case s of
    "good"    -> Good
    "warning" -> Warning
    "danger"  -> Danger
    color     -> ColorCode color
  parseJSON invalid = typeMismatch "Color" invalid

instance ToJSON Color where
  toJSON (ColorCode code) = Aeson.String code
  toJSON x = Aeson.String . Text.toLower . Text.pack . show $ x

data Attachment a = Attachment
  { _actions        :: Maybe [Action]
  , _attachmentType :: Maybe Text
  , _authorName     :: Maybe Text
  , _authorLink     :: Maybe Text
  , _authorIcon     :: Maybe Text
  , _callbackId     :: Maybe a
  , _color          :: Maybe Color
  , _fallback       :: Maybe Text
  , _fields         :: Maybe [Field]
  , _footer         :: Maybe Text
  , _footerIcon     :: Maybe Text
  , _imageUrl       :: Maybe Text
  , _pretext        :: Maybe Text
  , _text           :: Maybe Text
  , _thumbUrl       :: Maybe Text
  , _titleLink      :: Maybe Text
  , _title          :: Maybe Text
  , _ts             :: Maybe Timestamp
  } deriving (Eq, Generic, Ord, Read, Show)

makeLenses ''Attachment

instance FromJSON a => FromJSON (Attachment a) where
  parseJSON = genericParseJSON attachmentOptions

instance ToJSON a => ToJSON (Attachment a) where
  toJSON = genericToJSON attachmentOptions

attachmentOptions :: Options
attachmentOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 1
  , omitNothingFields  = True
  }

empty :: Attachment a
empty = Attachment
  { _actions        = Nothing
  , _attachmentType = Nothing
  , _authorIcon     = Nothing
  , _authorLink     = Nothing
  , _authorName     = Nothing
  , _callbackId     = Nothing
  , _color          = Nothing
  , _fallback       = Nothing
  , _fields         = Nothing
  , _footer         = Nothing
  , _footerIcon     = Nothing
  , _imageUrl       = Nothing
  , _pretext        = Nothing
  , _text           = Nothing
  , _thumbUrl       = Nothing
  , _title          = Nothing
  , _titleLink      = Nothing
  , _ts             = Nothing
  }
