module Taut.Client
     ( BotEvent(..)
     , MonadSlack
     , runBot
     , runSlack
     ) where

import Data.Aeson             ( Value )
import Data.Text              ( Text )
import Taut.Types.ChannelId   ( ChannelId )
import Taut.Types.EditInfo    ( EditInfo )
import Taut.Types.Timestamp   ( Timestamp )
import Taut.Types.UserId      ( UserId )
import Taut.Types.AccessToken ( AccessToken
                              , accessTokenText
                              )

data BotEvent
  = Message ChannelId UserId Text Timestamp (Maybe Subtype) (Maybe EditInfo)
  | OtherEvent Value
  deriving (Eq, Read, Show)

type Subtype = Text

class Monad m => MonadSlack m where

runSlack :: (AccessToken t, MonadSlack m) => t -> m a -> IO a
runSlack token = error "runSlack not impl"
  where
    _tokenText = accessTokenText token

runBot :: MonadSlack m => (BotEvent -> m a) -> m a
runBot _handler = error "runBot not impl"
