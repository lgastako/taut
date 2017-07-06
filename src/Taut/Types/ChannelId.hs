{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.ChannelId
       ( ChannelId
       , make
       , toText
       ) where

import Data.Aeson.TH ( defaultOptions
                     , deriveJSON
                     )
import Infinity

newtype ChannelId = ChannelId Text
  deriving (Eq, Generic, Ord, Read, Show)

make :: Text -> ChannelId
make = ChannelId

toText :: ChannelId -> Text
toText (ChannelId u) = u

$(deriveJSON defaultOptions ''ChannelId)
