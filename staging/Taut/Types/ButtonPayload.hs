{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.ButtonPayload
       ( ButtonPayload( ButtonPayload )
       , Channel( Channel )
       , Team( Team )
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
  { _teamDomain :: Text
  , _teamId     :: TeamId
  } deriving (Generic, Show)

makeLenses ''Team

instance FromJSON Team where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 5
    }

data Channel = Channel
  { _channelId   :: ChannelId
  , _channelName :: ChannelName
  } deriving (Generic, Show)

makeLenses ''Channel

instance FromJSON Channel where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 8
    }

data User = User
  { _userId   :: UserId
  , _userName :: UserName
  } deriving (Generic, Show)

makeLenses ''User

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 5
    }

newtype TriggerId = TriggerId Text
  deriving (Show, Generic)

instance FromJSON TriggerId

data ButtonPayload a = ButtonPayload
  { _actions         :: [Action]
  , _actionTs        :: Text
  , _attachmentId    :: Text
  , _callbackId      :: a
  , _channel         :: Channel
  , _messageTs       :: Text
  , _originalMessage :: Message a
  , _responseUrl     :: Text
  , _team            :: Team
  , _token           :: OauthToken
  , _triggerId       :: Maybe TriggerId
  , _user            :: User
  } deriving (Show, Generic)

makeLenses ''ButtonPayload

instance FromJSON a => FromJSON (ButtonPayload a) where
  parseJSON = genericParseJSON defaultOptions
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
