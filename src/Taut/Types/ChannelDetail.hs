{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.ChannelDetail
       ( ChannelDetail
       ) where

import Control.Lens               ( makeClassy )
import Taut.Types.Channel.Purpose ( Purpose )
import Taut.Types.Channel.Topic   ( Topic )
import Taut.Types.ChannelId       ( ChannelId )
import Taut.Types.ChannelName     ( ChannelName )
import Taut.Types.Timestamp       ( Timestamp )
import Taut.Types.UserId          ( UserId )

data ChannelDetail = ChannelDetail
  { _channelId  :: ChannelId
  , _name       :: ChannelName
  , _created    :: Timestamp
  , _creator    :: UserId
  , _isArchived :: Bool
  , _isGeneral  :: Bool
  , _members    :: [UserId]
  , _topic      :: Topic
  , _purpose    :: Purpose
  } deriving (Eq, Ord, Read, Show)

makeClassy ''ChannelDetail

-- {
--     "id": "C477H9JMQ",
--     "name": "ai-chamber",
--     "created": "1487368196",
--     "creator": "U0LPX9S06",
--     "is_archived": false,
--     "is_general": false,
--     "members": [
--         "U0LPX9S06",
--         "U0LTU4DTQ",
--         "U43KJMB5F",
--         "U4RRC2ZV2",
--         "U4W8FBZGD",
--         "U4W93Q8DB",
--         "U51T5SCFR",
--         "U5ESBUQV8"
--     ],
--     "topic": {
--         "value": "",
--         "creator": "",
--         "last_set": "0"
--     },
--     "purpose": {
--         "value": "For agents of various levels and types of sentience.",
--         "creator": "U0LPX9S06",
--         "last_set": "1487368239"
--     }
-- },
