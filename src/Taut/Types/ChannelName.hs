{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
module Taut.Types.ChannelName
       ( ChannelName
       , make
       ) where

import Data.Aeson.TH ( defaultOptions
                     , deriveJSON
                     )
import Infinity

newtype ChannelName = ChannelName Text
  deriving (Eq, Generic, Ord, Read, Show)

make :: Text -> ChannelName
make = ChannelName

$(deriveJSON defaultOptions ''ChannelName)
