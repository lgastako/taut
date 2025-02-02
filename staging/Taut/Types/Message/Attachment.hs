{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Taut.Types.Message.Attachment
     ( Attachment( Attachment )
     , Color( ColorCode
            , Danger
            , Good
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

import           Taut.Prelude                                   hiding ( decodeUtf8 )

import           Control.Lens                                          ( makeLenses )
import           Data.Aeson                                            ( FromJSON( parseJSON )
                                                                       , ToJSON( toJSON )
                                                                       , defaultOptions
                                                                       , eitherDecode
                                                                       , encode
                                                                       , genericParseJSON
                                                                       , genericToJSON
                                                                       )
import qualified Data.Aeson                           as Aeson
import           Data.Aeson.Types                                      ( Options( fieldLabelModifier
                                                                                , omitNothingFields
                                                                                )
                                                                       , camelTo2
                                                                       , typeMismatch
                                                                       )
import           Data.Default                                          ( Default
                                                                       , def
                                                                       )
import qualified Data.Text                            as Text
import qualified Data.Text.Lazy                       as LText
import           Data.Text.Lazy.Encoding                               ( decodeUtf8 )
import qualified Data.Text.Lazy.Encoding              as LTextE
import           Taut.Types.Message.Attachment.Action                  ( Action )
import           Taut.Types.Message.Attachment.Field                   ( Field )
import           Taut.Types.Timestamp                                  ( Timestamp )
import           Test.QuickCheck                                       ( Arbitrary
                                                                       , arbitrary
                                                                       , elements
                                                                       , genericShrink
                                                                       , shrink
                                                                       )
import           Web.HttpApiData                                       ( FromHttpApiData
                                                                       , ToHttpApiData
                                                                       , parseQueryParam
                                                                       , toQueryParam
                                                                       )

data Color
  = ColorCode Text
  | Danger
  | Good
  | Warning
  deriving (Data, Eq, Generic, Ord, Read, Show)

instance Arbitrary Color where
  arbitrary = do
    cc1 <- arbitrary
    cc2 <- arbitrary
    -- TODO: better
    elements [Good, Warning, Danger, ColorCode cc1, ColorCode cc2]
  shrink = genericShrink

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

data Attachment = Attachment
  { _actions        :: Maybe [Action]
  , _attachmentType :: Maybe Text
  , _authorName     :: Maybe Text
  , _authorLink     :: Maybe Text
  , _authorIcon     :: Maybe Text
  , _callbackId     :: Maybe Text
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
  } deriving (Data, Eq, Generic, Ord, Read, Show)

makeLenses ''Attachment

instance FromJSON Attachment where
  parseJSON = genericParseJSON attachmentOptions

instance ToJSON Attachment where
  toJSON = genericToJSON attachmentOptions

instance FromHttpApiData Attachment where
  parseQueryParam = first Text.pack . eitherDecode . LTextE.encodeUtf8 . LText.fromStrict

instance ToHttpApiData Attachment where
  toQueryParam = toStrict . decodeUtf8 . encode

attachmentOptions :: Options
attachmentOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 1
  , omitNothingFields  = True
  }

instance Arbitrary Attachment where
  arbitrary = Attachment
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Default Attachment where
  def = Attachment
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
