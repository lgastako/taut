{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Taut.Types.ButtonPayload
       ( ButtonPayload( ButtonPayload )
       , Channel( Channel )
       , Team( Team )
       , TriggerId( TriggerId )
       , User( User )
       , actions
       , actionTs
       , attachmentId
       , callbackId
       , channel
       , channelId
       , channelName
       , messageTs
       , originalMessage
       , responseUrl
       , team
       , teamDomain
       , teamId
       , token
       , triggerId
       , userId
       , userName
       , user
       ) where

import Control.Lens                         ( makeLenses )
import Data.Aeson                           ( FromJSON
                                            , ToJSON
                                            , defaultOptions
                                            , genericParseJSON
                                            , genericToJSON
                                            , parseJSON
                                            , toJSON
                                            )
import Data.Aeson.Types                     ( Options( fieldLabelModifier
                                                     , omitNothingFields
                                                     )
                                            , camelTo2
                                            )
import Focus.Prelude
import Taut.Types.ChannelId                 ( ChannelId )
import Taut.Types.ChannelName               ( ChannelName )
import Taut.Types.Message                   ( Message )
import Taut.Types.Message.Attachment.Action ( Action )
import Taut.Types.OauthToken                ( OauthToken )
import Taut.Types.TeamId                    ( TeamId )
import Taut.Types.UserId                    ( UserId )
import Taut.Types.UserName                  ( UserName )

data Team = Team
  { _teamDomain :: Text
  , _teamId     :: TeamId
  } deriving (Eq, Generic, Ord, Read, Show)

makeLenses ''Team

instance FromJSON Team where
  parseJSON = genericParseJSON teamOptions

instance ToJSON Team where
  toJSON = genericToJSON teamOptions

teamOptions :: Options
teamOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 5
  }

data Channel = Channel
  { _channelId   :: ChannelId
  , _channelName :: ChannelName
  } deriving (Eq, Generic, Ord, Read, Show)

makeLenses ''Channel

instance FromJSON Channel where
  parseJSON = genericParseJSON channelOptions

instance ToJSON Channel where
  toJSON = genericToJSON channelOptions

channelOptions :: Options
channelOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 8
  }

data User = User
  { _userId   :: UserId
  , _userName :: UserName
  } deriving (Eq, Generic, Ord, Read, Show)

makeLenses ''User

instance FromJSON User where
  parseJSON = genericParseJSON userOptions

instance ToJSON User where
  toJSON = genericToJSON userOptions

userOptions :: Options
userOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 5
  }

newtype TriggerId = TriggerId Text
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON TriggerId
instance ToJSON   TriggerId

data ButtonPayload = ButtonPayload
  { _actions         :: [Action]
  , _actionTs        :: Text
  , _attachmentId    :: Text
  , _callbackId      :: Text
  , _channel         :: Channel
  , _messageTs       :: Text
  , _originalMessage :: Message
  , _responseUrl     :: Text
  , _team            :: Team
  , _token           :: OauthToken
  , _triggerId       :: Maybe TriggerId
  , _user            :: User
  } deriving (Eq, Generic, Ord, Read, Show)

makeLenses ''ButtonPayload

instance FromJSON ButtonPayload where
  parseJSON = genericParseJSON buttonPayloadOptions

instance ToJSON ButtonPayload where
  toJSON = genericToJSON buttonPayloadOptions

buttonPayloadOptions :: Options
buttonPayloadOptions = defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 1
    , omitNothingFields  = True
    }

{-
-- An instance to use it with Servant
instance FromFormUrlEncoded ButtonPayload where
  fromFormUrlEncoded = maybe (Left "Expecting form with 'payload' field.")
                       (eitherDecode . cs)
                       . List.lookup "payload"
-}
