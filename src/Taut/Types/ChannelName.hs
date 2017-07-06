{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
module Taut.Types.ChannelName
       ( ChannelName
       , channelName
       , fromText
       , toText
       ) where

import Control.Lens  ( Iso'
                     , iso
                     )
import Data.Aeson.TH ( defaultOptions
                     , deriveJSON
                     )
import Infinity

newtype ChannelName = ChannelName Text
  deriving (Eq, Generic, Ord, Read, Show)

fromText :: Text -> ChannelName
fromText = ChannelName

toText :: ChannelName -> Text
toText (ChannelName n) = n

channelName :: Iso' ChannelName Text
channelName = iso toText fromText

$(deriveJSON defaultOptions ''ChannelName)
