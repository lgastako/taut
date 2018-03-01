{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Taut.Client
     ( BotEvent(..)
     , MonadSlack
     , runBot
     , runSlack
     ) where

import           Control.Monad.IO.Class       ( MonadIO
                                              , liftIO
                                              )
import           Data.Text                    ( unpack )
import           Taut.Conv                    ( conv )
import           Taut.Types.AccessToken       ( AccessToken
                                              , accessTokenText
                                              )
import           Taut.Types.BotEvent          ( BotEvent( Message
                                                        , OtherEvent
                                                        )
                                              )
import qualified Web.Slack              as WS

class Monad m => MonadSlack m where

data WrappedState a = WrappedState a
  deriving (Show)

runSlack :: (AccessToken t, MonadSlack m) => t -> m a -> IO a
runSlack token = error "runSlack not impl"
  where
    _tokenText = accessTokenText token

type BotEventHandler = BotEvent -> IO ()

runBot :: (AccessToken token, MonadIO m) => token -> BotEventHandler -> m ()
runBot token handler = liftIO $ WS.runBot config (botFrom handler) initState
  where
    config :: WS.SlackConfig
    config = WS.SlackConfig . unpack . accessTokenText $ token

    initState :: WrappedState ()
    initState = WrappedState ()

botFrom :: BotEventHandler -> WS.SlackBot (WrappedState ())
botFrom handler wsEvent = liftIO . handler $ botEvent
  where
    botEvent = conv wsEvent

    -- bot :: WS.SlackBot (WrappedState ())
    -- -- bot = liftIO . handler . (conv :: WS.Event -> BotEvent)
    -- -- bot wsEvent = do
    -- --   let botEvent :: BotEvent = conv wsEvent
    -- --   liftIO . handler $ botEvent
    -- bot wsEvent = liftIO . handler $ botEvent
    --   where
    --     -- botEvent = undefined :: BotEvent
    --     botEvent = (conv wsEvent) :: BotEvent
    --     _ = wsEvent :: WS.Event
