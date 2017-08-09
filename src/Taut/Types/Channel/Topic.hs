{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.Channel.Topic
       ( Topic(Topic)
       ) where

import Control.Lens         ( makeClassy )
import Data.Text            ( Text )
import Taut.Types.Timestamp ( Timestamp )
import Taut.Types.UserId    ( UserId )

data Topic = Topic
  { _creator :: Maybe UserId -- is blank when empty
  , _lastSet :: Timestamp
  , _value   :: Text
  } deriving (Eq, Ord, Read, Show)

makeClassy ''Topic -- probably will have to do manually to handle "" -> Nothing

-- NON-EMPTY EXAMPLE:
--
-- "topic": {
--     "value": "things that make other things better",
--     "creator": "U0LPX9S06",
--     "last_set": "1477500248"
-- }
--
-- EMPTY / NOT SET:
--
-- "topic": {
--     "value": "",
--     "creator": "",
--     "last_set": "0"
-- }
