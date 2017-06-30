module Taut
    ( ChannelAggregate
    , ChannelId
    , ChannelName
    , ChannelSet
    , EditInfo
    , MessageEvent
    , MessageType
    , Reaction
    , SubType
    , Timestamp
    , UserAggregate
    , UserName
    , UserId
    , UserSet
    , justMessages
    ) where

import Control.Lens                ( (^.) )
import Infinity
import Taut.Types.ChannelAggregate ( ChannelAggregate )
import Taut.Types.ChannelId        ( ChannelId )
import Taut.Types.ChannelName      ( ChannelName )
import Taut.Types.ChannelSet       ( ChannelSet )
import Taut.Types.EditInfo         ( EditInfo )
import Taut.Types.MessageEvent     ( MessageEvent
                                   , payload
                                   )
import Taut.Types.MessageType      ( MessageType )
import Taut.Types.Reaction         ( Reaction )
import Taut.Types.SubType          ( SubType )
import Taut.Types.Timestamp        ( Timestamp )
import Taut.Types.UserAggregate    ( UserAggregate )
import Taut.Types.UserId           ( UserId )
import Taut.Types.UserName         ( UserName )
import Taut.Types.UserSet          ( UserSet )

justMessages :: [MessageEvent (Maybe Text)] -> [MessageEvent Text]
justMessages = foldr add []
  where
    add e acc =
      case e ^. payload of
        Just text -> m:acc
          where
            m = const text <$> e
        Nothing -> acc
