{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.Channel.Purpose
       ( Purpose(Purpose)
       ) where

import Control.Lens         ( makeClassy )
import Data.Text            ( Text )
import Taut.Types.Timestamp ( Timestamp )
import Taut.Types.UserId    ( UserId )

data Purpose = Purpose
  { _creator :: Maybe UserId
  , _lastSet :: Timestamp
  , _value :: Text
  } deriving (Eq, Ord, Read, Show)

makeClassy ''Purpose -- probably will have to do manually to handle "" -> Nothing

-- NB. Presumably has the same empty string vs Nothing issue as Channel.Topic
--
--     "purpose": {
--         "value": "For agents of various levels and types of sentience.",
--         "creator": "U0LPX9S06",
--         "last_set": "1487368239"
--     }
