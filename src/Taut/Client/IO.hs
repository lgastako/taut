{-# LANGUAGE TupleSections #-}
module Taut.Client.IO
     ( sendMessageIO
     ) where

import           Control.Monad                       ( void )
import qualified Data.Map.Strict        as Map
import           Data.Text                           ( Text
                                                     , unpack
                                                     )
import           Network.HTTP.Base                   ( urlEncode )
import           Taut.Types.AccessToken              ( AccessToken
                                                     , accessTokenString
                                                     )
import           Taut.Types.ChannelId                ( ChannelId )
import qualified Taut.Types.ChannelId   as ChannelId
import qualified Web.Slack.Api          as WSA

-- TODO: Eliminate dependencies on utf8-string and Network.HTTP.Base

sendMessageIO :: AccessToken token => token -> ChannelId -> Text -> IO ()
sendMessageIO token chanId msg = void . WSA.runSlack tokenStr $ action
  where
    action :: WSA.Slack WSA.TimeStamp
    action = WSA.request "chat.postMessage" args

    args = Map.fromList . concat $
      [ maybe [] ((:[]) . ("text",) . urlEncode . unpack) (Just msg)
      , [ ("channel", unpack . ChannelId.toText $ chanId ) ]
      ]

    tokenStr = accessTokenString token
