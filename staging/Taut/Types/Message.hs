{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Taut.Types.Message
       ( Message( Message )
       , Parse( Full
              , None
              )
       , asUser
       , attachments
       , botId
       , channel
       , iconEmoji
       , iconUrl
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
       ) where

import Focus.Prelude                 hiding ( empty )

import Control.Lens                         ( makeLenses )
import Data.Aeson                           ( FromJSON( parseJSON )
                                            , ToJSON( toJSON )
                                            , defaultOptions
                                            , genericParseJSON
                                            , genericToJSON
                                            )
import Data.Aeson.Types                     ( Options( constructorTagModifier
                                                     , fieldLabelModifier
                                                     , omitNothingFields
                                                     )
                                            , camelTo2
                                            )
import Data.Char                            ( toLower )
import Data.Default                         ( Default
                                            , def
                                            )
import Taut.Types.ChannelId                 ( ChannelId )
import Taut.Types.Message.Attachment        ( Attachment )
import Taut.Types.OauthToken                ( OauthToken )
import Taut.Types.UserName                  ( UserName )

data Parse = Full | None
  deriving (Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON Parse where
  parseJSON = genericParseJSON parseOptions

instance ToJSON Parse where
  toJSON = genericToJSON parseOptions

parseOptions :: Options
parseOptions = defaultOptions
  { constructorTagModifier = fmap toLower
  }

data Message = Message
  { _asUser         :: Maybe Bool
  , _attachments    :: Maybe [Attachment]
  , _botId          :: Maybe Text
  , _channel        :: Maybe ChannelId
  , _iconEmoji      :: Maybe Text
  , _iconUrl        :: Maybe Text
  , _linkNames      :: Maybe Bool
  , _parse          :: Maybe Parse
  , _replyBroadcast :: Maybe Bool
  , _subType        :: Maybe Text
  , _text           :: Maybe Text
  , _threadTs       :: Maybe Text
  , _token          :: Maybe OauthToken
  , _ts             :: Maybe Text
  , _type'          :: Maybe Text
  , _unfurlLinks    :: Maybe Bool
  , _unfurlMedia    :: Maybe Bool
  , _username       :: Maybe UserName
  } deriving (Eq, Generic, Ord, Read, Show)

makeLenses ''Message

instance FromJSON Message where
  parseJSON = genericParseJSON messageOptions

instance ToJSON Message where
  toJSON = genericToJSON messageOptions

messageOptions :: Options
messageOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 1 . filter (/='\'')
  , omitNothingFields = True
  }

instance Default Message where
  def = Message
    { _asUser         = Nothing
    , _attachments    = Nothing
    , _botId          = Nothing
    , _channel        = Nothing
    , _iconEmoji      = Nothing
    , _iconUrl        = Nothing
    , _linkNames      = Nothing
    , _parse          = Nothing
    , _replyBroadcast = Nothing
    , _subType        = Nothing
    , _text           = Nothing
    , _threadTs       = Nothing
    , _token          = Nothing
    , _ts             = Nothing
    , _type'          = Nothing
    , _unfurlLinks    = Nothing
    , _unfurlMedia    = Nothing
    , _username       = Nothing
    }
