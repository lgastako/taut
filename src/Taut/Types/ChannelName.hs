{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Taut.Types.ChannelName
       ( ChannelName
       , channelName
       , fromText
       , toText
       ) where

import Focus.Prelude

import Control.Lens              ( Iso'
                                 , iso
                                 )
import Data.Aeson.TH             ( defaultOptions
                                 , deriveJSON
                                 )
import Data.DeriveTH             ( derive
                                 , makeArbitrary
                                 )
import Test.QuickCheck           ( Arbitrary
                                 , arbitrary
                                 )
import Test.QuickCheck.Instances ()

newtype ChannelName = ChannelName Text
  deriving (Eq, Generic, Ord, Read, Show)

fromText :: Text -> ChannelName
fromText = ChannelName

toText :: ChannelName -> Text
toText (ChannelName n) = n

channelName :: Iso' ChannelName Text
channelName = iso toText fromText

$(deriveJSON defaultOptions ''ChannelName)

derive makeArbitrary ''ChannelName
