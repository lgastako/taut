{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.ButtonPayload
       ( Team(Team)
       , teamId
       , teamDomain
       , Channel(Channel)
       , channelId
       , channelName
       , User(User)
       , userId
       , userName
       , ButtonPayload(ButtonPayload)
       , actions
       , callbackId
       , team
       , channel
       , user
       , actionTs
       , messageTs
       , attachmentId
       , token
       , triggerId
       , originalMessage
       , responseUrl
       )
       where

import Control.Lens                         ( makeLenses )
import Data.Aeson                           ( FromJSON
                                            , defaultOptions
                                            , genericParseJSON
                                            , parseJSON
                                            )
import Data.Aeson.Types                     ( Options( fieldLabelModifier
                                                     , omitNothingFields
                                                     )
                                            , camelTo2
                                            )
import Data.Text                            ( Text )
import GHC.Generics                         ( Generic )
import Taut.Types.ChannelId                 ( ChannelId )
import Taut.Types.ChannelName               ( ChannelName )
import Taut.Types.Message                   ( Message )
import Taut.Types.Message.Attachment.Action ( Action )
import Taut.Types.OauthToken                ( OauthToken )
import Taut.Types.TeamId                    ( TeamId )
import Taut.Types.UserId                    ( UserId )
import Taut.Types.UserName                  ( UserName )

data Team = Team
  { _teamId :: TeamId
  , _teamDomain :: Text
  }
  deriving (Generic, Show)
makeLenses ''Team

customTeamOptions :: Options
customTeamOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 5
  }

instance FromJSON Team where
  parseJSON = genericParseJSON customTeamOptions

data Channel = Channel
  { _channelId :: ChannelId
  , _channelName :: ChannelName
  }
  deriving (Generic, Show)
makeLenses ''Channel

customChannelOptions :: Options
customChannelOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 8
  }

instance FromJSON Channel where
  parseJSON = genericParseJSON customChannelOptions

data User = User
  { _userId :: UserId
  , _userName :: UserName
  }
  deriving (Generic, Show)
makeLenses ''User

customUserOptions :: Options
customUserOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 5
  }

instance FromJSON User where
  parseJSON = genericParseJSON customUserOptions

newtype TriggerId = TriggerId Text
                    deriving (Show, Generic)

instance FromJSON TriggerId

data ButtonPayload a = ButtonPayload
  { _actions :: [Action]
  , _callbackId :: a
  , _team :: Team
  , _channel :: Channel
  , _user :: User
  , _actionTs :: Text
  , _messageTs :: Text
  , _attachmentId :: Text
  , _token :: OauthToken
  , _originalMessage :: Message a
  , _responseUrl :: Text
  , _triggerId :: Maybe TriggerId
  }
  deriving (Show, Generic)
makeLenses ''ButtonPayload

instance FromJSON a => FromJSON (ButtonPayload a) where
  parseJSON = genericParseJSON customOptions

customOptions :: Options
customOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 1
  , omitNothingFields = True
  }

{-
-- An instance to use it with Servant
instance FromFormUrlEncoded ButtonPayload where
  fromFormUrlEncoded = maybe (Left "Expecting form with 'payload' field.")
                       (eitherDecode . cs)
                       . List.lookup "payload"
-}
