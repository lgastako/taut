{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.Message
       ( Parse( Full, None )
       , Message( Message )
       , token
       , botId
       , channel
       , text
       , parse
       , linkNames
       , attachments
       , unfurlLinks
       , unfurlMedia
       , username
       , asUser
       , iconUrl
       , iconEmoji
       , threadTs
       , replyBroadcast
       , type'
       , subType
       , ts
       , empty
       )
       where

import Control.Lens                  ( makeLenses )
import Data.Aeson                    ( FromJSON( parseJSON )
                                     , ToJSON( toJSON )
                                     , defaultOptions
                                     , genericParseJSON
                                     , genericToJSON
                                     )
import Data.Aeson.Types              ( Options( constructorTagModifier
                                              , fieldLabelModifier
                                              , omitNothingFields
                                              )
                                     , camelTo2
                                     )
import Data.Char                     ( toLower )
import Data.Text                     ( Text )
import GHC.Generics                  ( Generic )
import Taut.Types.ChannelId          ( ChannelId )
import Taut.Types.Message.Attachment ( Attachment )
import Taut.Types.OauthToken         ( OauthToken )
import Taut.Types.UserName           ( UserName )

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
  { _token :: Maybe OauthToken
  , _botId :: Maybe Text
  , _channel :: Maybe ChannelId
  , _text :: Maybe Text
  , _parse :: Maybe Parse
  , _linkNames :: Maybe Bool
  , _attachments :: Maybe [Attachment a]
  , _unfurlLinks :: Maybe Bool
  , _unfurlMedia :: Maybe Bool
  , _username :: Maybe UserName
  , _asUser :: Maybe Bool
  , _iconUrl :: Maybe Text
  , _iconEmoji :: Maybe Text
  , _threadTs :: Maybe Text
  , _replyBroadcast :: Maybe Bool
  , _type' :: Maybe Text
  , _subType :: Maybe Text
  , _ts :: Maybe Text
  }
  deriving (Show, Generic)

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

empty :: Message a
empty = Message
  { _token = Nothing
  , _botId = Nothing
  , _channel = Nothing
  , _text = Nothing
  , _parse = Nothing
  , _linkNames = Nothing
  , _attachments = Nothing
  , _unfurlLinks = Nothing
  , _unfurlMedia = Nothing
  , _username = Nothing
  , _asUser = Nothing
  , _iconUrl = Nothing
  , _iconEmoji = Nothing
  , _threadTs = Nothing
  , _replyBroadcast = Nothing
  , _type' = Nothing
  , _subType = Nothing
  , _ts = Nothing
  }
