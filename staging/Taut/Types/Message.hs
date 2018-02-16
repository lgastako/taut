{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Slack.Types.Message
       ( Message( Message )
       , Parse( Full
              , None
              )
       , asUser
       , attachments
       , botId
       , channel
       , iconUrl
       , iconEmoji
       , linkNames
       , parse
       , replyBroadcast
       , subType
       , text
       , threadTs
       , token
       , ts
       , type'
       , unfurlLinks
       , unfurlMedia
       , username
       )
       where

import Control.Lens                   ( makeLenses )
import Data.Aeson                     ( FromJSON( parseJSON )
                                      , ToJSON( toJSON )
                                      , defaultOptions
                                      , genericParseJSON
                                      , genericToJSON
                                      )
import Data.Aeson.Types               ( Options( constructorTagModifier
                                               , fieldLabelModifier
                                               , omitNothingFields
                                               )
                                      , camelTo2
                                      )
import Data.Char                      ( toLower )
import Data.Default                   ( Default
                                      , def
                                      )
import Focus.Prelude
import Taut.Types.Message.Attachment ( Attachment )
import Taut.Types.ChannelId           ( ChannelId )
import Taut.Types.OauthToken          ( OauthToken )
import Taut.Types.UserName            ( UserName )

data Parse = Full | None
  deriving (Generic, Show)

instance ToJSON Parse where
  toJSON = genericToJSON customUnionTypeOptions

instance FromJSON Parse where
  parseJSON = genericParseJSON customUnionTypeOptions

customUnionTypeOptions :: Options
customUnionTypeOptions = defaultOptions
  { constructorTagModifier = fmap toLower
  }

data Message a = Message
  { _token          :: Maybe OauthToken
  , _botId          :: Maybe Text
  , _channel        :: Maybe ChannelId
  , _text           :: Maybe Text
  , _parse          :: Maybe Parse
  , _linkNames      :: Maybe Bool
  , _attachments    :: Maybe [Attachment a]
  , _unfurlLinks    :: Maybe Bool
  , _unfurlMedia    :: Maybe Bool
  , _username       :: Maybe UserName
  , _asUser         :: Maybe Bool
  , _iconUrl        :: Maybe Text
  , _iconEmoji      :: Maybe Text
  , _threadTs       :: Maybe Text
  , _replyBroadcast :: Maybe Bool
  , _type'          :: Maybe Text
  , _subType        :: Maybe Text
  , _ts             :: Maybe Text
  } deriving (Show, Generic)

makeLenses ''Message

instance ToJSON a => ToJSON (Message a) where
  toJSON = genericToJSON customOptions

instance FromJSON a => FromJSON (Message a) where
  parseJSON = genericParseJSON customOptions

customOptions :: Options
customOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 1 . filter (/='\'')
  , omitNothingFields = True
  }

instance Default (Message a) where
  def = Message
    { _asUser         = Nothing
    , _attachments    = Nothing
    , _botId          = Nothing
    , _channel        = Nothing
    , _iconEmoji      = Nothing
    , _iconUrl        = Nothing
    , _linkNames      = Nothing
    , _parse          = Nothing
    , _text           = Nothing
    , _token          = Nothing
    , _unfurlLinks    = Nothing
    , _unfurlMedia    = Nothing
    , _username       = Nothing
    , _replyBroadcast = Nothing
    , _subType        = Nothing
    , _threadTs       = Nothing
    , _ts             = Nothing
    , _type'          = Nothing
    }

